(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-LAND (err u101))
(define-constant ERR-ALREADY-REGISTERED (err u102))
(define-constant ERR-NOT-FOUND (err u103))
(define-constant ERR-TRANSFER-FAILED (err u104))

(define-data-var registry-admin principal tx-sender)

(define-map land-records
  { land-id: uint }
  {
    owner: principal,
    coordinates: (list 8 uint),
    area: uint,
    registration-date: uint,
    last-transfer-date: uint,
    land-type: (string-ascii 20),
    status: (string-ascii 10)
  }
)

(define-map land-history
  { land-id: uint, transaction-id: uint }
  {
    previous-owner: principal,
    new-owner: principal,
    transfer-date: uint,
    transaction-type: (string-ascii 20)
  }
)

(define-data-var transaction-counter uint u0)

(define-public (register-land (land-id uint) (coordinates (list 8 uint)) (area uint) (land-type (string-ascii 20)))
  (let ((current-owner tx-sender))
    (asserts! (is-eq tx-sender (var-get registry-admin)) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? land-records { land-id: land-id })) ERR-ALREADY-REGISTERED)
    (ok (map-set land-records
      { land-id: land-id }
      {
        owner: current-owner,
        coordinates: coordinates,
        area: area,
        registration-date: stacks-block-height,
        last-transfer-date: stacks-block-height,
        land-type: land-type,
        status: "active"
      }
    ))
  )
)

(define-public (transfer-land (land-id uint) (new-owner principal))
  (let (
    (current-owner tx-sender)
    (land-record (unwrap! (map-get? land-records { land-id: land-id }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq current-owner (get owner land-record)) ERR-NOT-AUTHORIZED)
    (var-set transaction-counter (+ (var-get transaction-counter) u1))
    (map-set land-history
      { land-id: land-id, transaction-id: (var-get transaction-counter) }
      {
        previous-owner: current-owner,
        new-owner: new-owner,
        transfer-date: stacks-block-height,
        transaction-type: "transfer"
      }
    )
    (ok (map-set land-records
      { land-id: land-id }
      (merge land-record { 
        owner: new-owner,
        last-transfer-date: stacks-block-height
      })
    ))
  )
)

(define-public (update-land-status (land-id uint) (new-status (string-ascii 10)))
  (let ((land-record (unwrap! (map-get? land-records { land-id: land-id }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (var-get registry-admin)) ERR-NOT-AUTHORIZED)
    (ok (map-set land-records
      { land-id: land-id }
      (merge land-record { status: new-status })
    ))
  )
)

(define-read-only (get-land-details (land-id uint))
  (ok (unwrap! (map-get? land-records { land-id: land-id }) ERR-NOT-FOUND))
)

(define-read-only (get-land-history (land-id uint))
  (ok (map-get? land-history { land-id: land-id, transaction-id: (var-get transaction-counter) }))
)

(define-read-only (get-owner (land-id uint))
  (ok (get owner (unwrap! (map-get? land-records { land-id: land-id }) ERR-NOT-FOUND)))
)

(define-public (change-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get registry-admin)) ERR-NOT-AUTHORIZED)
    (ok (var-set registry-admin new-admin))
  )
)

(define-read-only (verify-ownership (land-id uint) (owner principal))
  (let ((land-record (unwrap! (map-get? land-records { land-id: land-id }) ERR-NOT-FOUND)))
    (ok (is-eq owner (get owner land-record)))
  )
)

(define-public (update-coordinates (land-id uint) (new-coordinates (list 8 uint)))
  (let ((land-record (unwrap! (map-get? land-records { land-id: land-id }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (var-get registry-admin)) ERR-NOT-AUTHORIZED)
    (ok (map-set land-records
      { land-id: land-id }
      (merge land-record { coordinates: new-coordinates })
    ))
  )
)

(define-read-only (get-transaction-count)
  (ok (var-get transaction-counter))
)
