(define-constant SELF (as-contract tx-sender))
(define-constant STATUS-OPEN u0)
(define-constant STATUS-LOCKED u1)
(define-constant STATUS-CANCELLED u2)
(define-constant STATUS-SETTLED u3)
(define-constant STATUS-EXPIRED u4)
(define-constant ERR-NOT-SELLER (err u100))
(define-constant ERR-NOT-BUYER (err u101))
(define-constant ERR-NOT-OPEN (err u102))
(define-constant ERR-NOT-LOCKED (err u103))
(define-constant ERR-NO-SALE (err u104))
(define-constant ERR-BUYER-NOT-ALLOWED (err u105))
(define-constant ERR-NO-DEADLINE (err u106))
(define-constant ERR-NOT-EXPIRED (err u107))

(define-data-var next-sale-id uint u1)

(define-map sales
  {id: uint}
  {
    seller: principal,
    buyer: (optional principal),
    asset-id: (buff 64),
    price: uint,
    deposit: uint,
    locked-by: (optional principal),
    deadline: (optional uint),
    status: uint
  }
)

(define-read-only (get-sale (id uint))
  (map-get? sales {id: id})
)

(define-read-only (get-next-sale-id)
  (var-get next-sale-id)
)

(define-public (create-sale (asset-id (buff 64)) (price uint) (buyer (optional principal)) (deadline (optional uint)))
  (let
    ((id (var-get next-sale-id)))
    (begin
      (map-set sales {id: id} {seller: tx-sender, buyer: buyer, asset-id: asset-id, price: price, deposit: u0, locked-by: none, deadline: deadline, status: STATUS-OPEN})
      (var-set next-sale-id (+ id u1))
      (ok id)
    )
  )
)

(define-public (lock-escrow (id uint))
  (let
    ((sale (unwrap! (map-get? sales {id: id}) ERR-NO-SALE)))
    (asserts! (is-eq (get status sale) STATUS-OPEN) ERR-NOT-OPEN)
    (let
      ((price (get price sale)))
      (match (get buyer sale)
        buyer-p
        (asserts! (is-eq tx-sender buyer-p) ERR-BUYER-NOT-ALLOWED)
        true
      )
      (try! (stx-transfer? price tx-sender SELF))
      (map-set sales {id: id} (merge sale {deposit: price, locked-by: (some tx-sender), status: STATUS-LOCKED}))
      (ok true)
    )
  )
)

(define-public (finalize (id uint))
  (let 
    ((sale (unwrap! (map-get? sales {id: id}) ERR-NO-SALE)))
    (asserts! (is-eq (get status sale) STATUS-LOCKED) ERR-NOT-LOCKED)
    (let
      ((locker-p (unwrap! (get locked-by sale) ERR-NOT-LOCKED))
       (amount (get deposit sale))
       (seller (get seller sale)))
      (asserts! (is-eq locker-p tx-sender) ERR-NOT-BUYER)
      (as-contract (try! (stx-transfer? amount tx-sender seller)))
      (map-set sales {id: id} (merge sale {deposit: u0, locked-by: none, status: STATUS-SETTLED}))
      (ok true)
    )
  )
)

(define-public (cancel (id uint))
  (let 
    ((sale (unwrap! (map-get? sales {id: id}) ERR-NO-SALE)))
    (asserts! (is-eq (get seller sale) tx-sender) ERR-NOT-SELLER)
    (asserts! (is-eq (get status sale) STATUS-OPEN) ERR-NOT-OPEN)
    (map-set sales {id: id} (merge sale {status: STATUS-CANCELLED}))
    (ok true)
  )
)

(define-public (expire (id uint))
  (let 
    ((sale (unwrap! (map-get? sales {id: id}) ERR-NO-SALE)))
    (asserts! (is-eq (get status sale) STATUS-LOCKED) ERR-NOT-LOCKED)
    (let
      ((d (unwrap! (get deadline sale) ERR-NO-DEADLINE))
       (locker-p (unwrap! (get locked-by sale) ERR-NOT-LOCKED))
       (amount (get deposit sale)))
      (asserts! (> stacks-block-height d) ERR-NOT-EXPIRED)
      (as-contract (try! (stx-transfer? amount tx-sender locker-p)))
      (map-set sales {id: id} (merge sale {deposit: u0, locked-by: none, status: STATUS-EXPIRED}))
      (ok true)
    )
  )
)
