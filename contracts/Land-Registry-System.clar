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
    status: (string-ascii 10),
  }
)

(define-map land-history
  {
    land-id: uint,
    transaction-id: uint,
  }
  {
    previous-owner: principal,
    new-owner: principal,
    transfer-date: uint,
    transaction-type: (string-ascii 20),
  }
)

(define-data-var transaction-counter uint u0)

(define-public (register-land
    (land-id uint)
    (coordinates (list 8 uint))
    (area uint)
    (land-type (string-ascii 20))
  )
  (let ((current-owner tx-sender))
    (asserts! (is-eq tx-sender (var-get registry-admin)) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? land-records { land-id: land-id }))
      ERR-ALREADY-REGISTERED
    )
    (ok (map-set land-records { land-id: land-id } {
      owner: current-owner,
      coordinates: coordinates,
      area: area,
      registration-date: stacks-block-height,
      last-transfer-date: stacks-block-height,
      land-type: land-type,
      status: "active",
    }))
  )
)

(define-public (transfer-land
    (land-id uint)
    (new-owner principal)
  )
  (let (
      (current-owner tx-sender)
      (land-record (unwrap! (map-get? land-records { land-id: land-id }) ERR-NOT-FOUND))
    )
    (asserts! (is-eq current-owner (get owner land-record)) ERR-NOT-AUTHORIZED)
    (var-set transaction-counter (+ (var-get transaction-counter) u1))
    (map-set land-history {
      land-id: land-id,
      transaction-id: (var-get transaction-counter),
    } {
      previous-owner: current-owner,
      new-owner: new-owner,
      transfer-date: stacks-block-height,
      transaction-type: "transfer",
    })
    (ok (map-set land-records { land-id: land-id }
      (merge land-record {
        owner: new-owner,
        last-transfer-date: stacks-block-height,
      })
    ))
  )
)

(define-public (update-land-status
    (land-id uint)
    (new-status (string-ascii 10))
  )
  (let ((land-record (unwrap! (map-get? land-records { land-id: land-id }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (var-get registry-admin)) ERR-NOT-AUTHORIZED)
    (ok (map-set land-records { land-id: land-id }
      (merge land-record { status: new-status })
    ))
  )
)

(define-read-only (get-land-details (land-id uint))
  (ok (unwrap! (map-get? land-records { land-id: land-id }) ERR-NOT-FOUND))
)

(define-read-only (get-land-history (land-id uint))
  (ok (map-get? land-history {
    land-id: land-id,
    transaction-id: (var-get transaction-counter),
  }))
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

(define-read-only (verify-ownership
    (land-id uint)
    (owner principal)
  )
  (let ((land-record (unwrap! (map-get? land-records { land-id: land-id }) ERR-NOT-FOUND)))
    (ok (is-eq owner (get owner land-record)))
  )
)

(define-public (update-coordinates
    (land-id uint)
    (new-coordinates (list 8 uint))
  )
  (let ((land-record (unwrap! (map-get? land-records { land-id: land-id }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (var-get registry-admin)) ERR-NOT-AUTHORIZED)
    (ok (map-set land-records { land-id: land-id }
      (merge land-record { coordinates: new-coordinates })
    ))
  )
)

(define-read-only (get-transaction-count)
  (ok (var-get transaction-counter))
)

(define-constant ERR-INVALID-PRICE (err u105))
(define-constant ERR-UNAUTHORIZED-APPRAISER (err u106))

(define-map authorized-appraisers
  { appraiser: principal }
  {
    authorized: bool,
    certification-date: uint,
  }
)

(define-map land-valuations
  {
    land-id: uint,
    valuation-id: uint,
  }
  {
    appraiser: principal,
    market-value: uint,
    assessed-value: uint,
    valuation-date: uint,
    valuation-method: (string-ascii 20),
    currency: (string-ascii 10),
  }
)

(define-data-var valuation-counter uint u0)

(define-public (authorize-appraiser (appraiser principal))
  (begin
    (asserts! (is-eq tx-sender (var-get registry-admin)) ERR-NOT-AUTHORIZED)
    (ok (map-set authorized-appraisers { appraiser: appraiser } {
      authorized: true,
      certification-date: stacks-block-height,
    }))
  )
)

(define-public (revoke-appraiser (appraiser principal))
  (begin
    (asserts! (is-eq tx-sender (var-get registry-admin)) ERR-NOT-AUTHORIZED)
    (ok (map-set authorized-appraisers { appraiser: appraiser } {
      authorized: false,
      certification-date: stacks-block-height,
    }))
  )
)

(define-public (add-land-valuation
    (land-id uint)
    (market-value uint)
    (assessed-value uint)
    (valuation-method (string-ascii 20))
    (currency (string-ascii 10))
  )
  (let (
      (appraiser-record (unwrap! (map-get? authorized-appraisers { appraiser: tx-sender })
        ERR-UNAUTHORIZED-APPRAISER
      ))
      (land-exists (is-some (map-get? land-records { land-id: land-id })))
    )
    (asserts! land-exists ERR-NOT-FOUND)
    (asserts! (get authorized appraiser-record) ERR-UNAUTHORIZED-APPRAISER)
    (asserts! (> market-value u0) ERR-INVALID-PRICE)
    (asserts! (> assessed-value u0) ERR-INVALID-PRICE)
    (var-set valuation-counter (+ (var-get valuation-counter) u1))
    (ok (map-set land-valuations {
      land-id: land-id,
      valuation-id: (var-get valuation-counter),
    } {
      appraiser: tx-sender,
      market-value: market-value,
      assessed-value: assessed-value,
      valuation-date: stacks-block-height,
      valuation-method: valuation-method,
      currency: currency,
    }))
  )
)

(define-read-only (get-latest-valuation (land-id uint))
  (ok (map-get? land-valuations {
    land-id: land-id,
    valuation-id: (var-get valuation-counter),
  }))
)

(define-read-only (get-specific-valuation
    (land-id uint)
    (valuation-id uint)
  )
  (ok (map-get? land-valuations {
    land-id: land-id,
    valuation-id: valuation-id,
  }))
)

(define-read-only (is-authorized-appraiser (appraiser principal))
  (match (map-get? authorized-appraisers { appraiser: appraiser })
    appraiser-data (ok (get authorized appraiser-data))
    (ok false)
  )
)

(define-read-only (get-valuation-count)
  (ok (var-get valuation-counter))
)

(define-constant ERR-DISPUTE-EXISTS (err u107))
(define-constant ERR-DISPUTE-NOT-FOUND (err u108))
(define-constant ERR-INVALID-STATUS (err u109))
(define-constant ERR-UNAUTHORIZED-RESOLVER (err u110))

(define-map land-disputes
  { dispute-id: uint }
  {
    land-id: uint,
    complainant: principal,
    respondent: principal,
    dispute-type: (string-ascii 30),
    description: (string-ascii 500),
    filing-date: uint,
    status: (string-ascii 20),
    resolver: (optional principal),
    resolution-date: (optional uint),
    resolution-details: (optional (string-ascii 500)),
  }
)

(define-map authorized-resolvers
  { resolver: principal }
  {
    authorized: bool,
    specialization: (string-ascii 50),
  }
)

(define-data-var dispute-counter uint u0)

(define-public (authorize-resolver
    (resolver principal)
    (specialization (string-ascii 50))
  )
  (begin
    (asserts! (is-eq tx-sender (var-get registry-admin)) ERR-NOT-AUTHORIZED)
    (ok (map-set authorized-resolvers { resolver: resolver } {
      authorized: true,
      specialization: specialization,
    }))
  )
)

(define-public (file-dispute
    (land-id uint)
    (respondent principal)
    (dispute-type (string-ascii 30))
    (description (string-ascii 500))
  )
  (let ((land-exists (is-some (map-get? land-records { land-id: land-id }))))
    (asserts! land-exists ERR-NOT-FOUND)
    (var-set dispute-counter (+ (var-get dispute-counter) u1))
    (ok (map-set land-disputes { dispute-id: (var-get dispute-counter) } {
      land-id: land-id,
      complainant: tx-sender,
      respondent: respondent,
      dispute-type: dispute-type,
      description: description,
      filing-date: stacks-block-height,
      status: "filed",
      resolver: none,
      resolution-date: none,
      resolution-details: none,
    }))
  )
)

(define-public (assign-resolver
    (dispute-id uint)
    (resolver principal)
  )
  (let (
      (dispute-record (unwrap! (map-get? land-disputes { dispute-id: dispute-id })
        ERR-DISPUTE-NOT-FOUND
      ))
      (resolver-record (unwrap! (map-get? authorized-resolvers { resolver: resolver })
        ERR-UNAUTHORIZED-RESOLVER
      ))
    )
    (asserts! (is-eq tx-sender (var-get registry-admin)) ERR-NOT-AUTHORIZED)
    (asserts! (get authorized resolver-record) ERR-UNAUTHORIZED-RESOLVER)
    (asserts! (is-eq (get status dispute-record) "filed") ERR-INVALID-STATUS)
    (ok (map-set land-disputes { dispute-id: dispute-id }
      (merge dispute-record {
        resolver: (some resolver),
        status: "assigned",
      })
    ))
  )
)

(define-public (resolve-dispute
    (dispute-id uint)
    (resolution-details (string-ascii 500))
  )
  (let (
      (dispute-record (unwrap! (map-get? land-disputes { dispute-id: dispute-id })
        ERR-DISPUTE-NOT-FOUND
      ))
      (assigned-resolver (unwrap! (get resolver dispute-record) ERR-UNAUTHORIZED-RESOLVER))
    )
    (asserts! (is-eq tx-sender assigned-resolver) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status dispute-record) "assigned") ERR-INVALID-STATUS)
    (ok (map-set land-disputes { dispute-id: dispute-id }
      (merge dispute-record {
        status: "resolved",
        resolution-date: (some stacks-block-height),
        resolution-details: (some resolution-details),
      })
    ))
  )
)

(define-public (update-dispute-status
    (dispute-id uint)
    (new-status (string-ascii 20))
  )
  (let ((dispute-record (unwrap! (map-get? land-disputes { dispute-id: dispute-id })
      ERR-DISPUTE-NOT-FOUND
    )))
    (asserts! (is-eq tx-sender (var-get registry-admin)) ERR-NOT-AUTHORIZED)
    (ok (map-set land-disputes { dispute-id: dispute-id }
      (merge dispute-record { status: new-status })
    ))
  )
)

(define-read-only (get-dispute-details (dispute-id uint))
  (ok (unwrap! (map-get? land-disputes { dispute-id: dispute-id })
    ERR-DISPUTE-NOT-FOUND
  ))
)

(define-read-only (get-disputes-by-land (land-id uint))
  (ok land-id)
)

(define-read-only (is-authorized-resolver (resolver principal))
  (match (map-get? authorized-resolvers { resolver: resolver })
    resolver-data (ok (get authorized resolver-data))
    (ok false)
  )
)

(define-read-only (get-dispute-count)
  (ok (var-get dispute-counter))
)

(define-constant ERR-LEASE-EXISTS (err u111))
(define-constant ERR-LEASE-NOT-FOUND (err u112))
(define-constant ERR-LEASE-EXPIRED (err u113))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u114))
(define-constant ERR-LEASE-NOT-ACTIVE (err u115))

(define-map land-leases
  { lease-id: uint }
  {
    land-id: uint,
    landlord: principal,
    tenant: principal,
    monthly-rent: uint,
    security-deposit: uint,
    lease-start: uint,
    lease-end: uint,
    payment-due-date: uint,
    last-payment-date: uint,
    status: (string-ascii 15),
    lease-purpose: (string-ascii 30),
  }
)

(define-map lease-payments
  {
    lease-id: uint,
    payment-id: uint,
  }
  {
    amount: uint,
    payment-date: uint,
    payment-type: (string-ascii 20),
    late-fee: uint,
  }
)

(define-data-var lease-counter uint u0)
(define-data-var payment-counter uint u0)

(define-public (create-lease
    (land-id uint)
    (tenant principal)
    (monthly-rent uint)
    (security-deposit uint)
    (lease-duration-blocks uint)
    (lease-purpose (string-ascii 30))
  )
  (let (
      (land-record (unwrap! (map-get? land-records { land-id: land-id }) ERR-NOT-FOUND))
      (lease-start-block stacks-block-height)
      (lease-end-block (+ stacks-block-height lease-duration-blocks))
    )
    (asserts! (is-eq tx-sender (get owner land-record)) ERR-NOT-AUTHORIZED)
    (asserts! (> monthly-rent u0) ERR-INVALID-PRICE)
    (var-set lease-counter (+ (var-get lease-counter) u1))
    (ok (map-set land-leases { lease-id: (var-get lease-counter) } {
      land-id: land-id,
      landlord: tx-sender,
      tenant: tenant,
      monthly-rent: monthly-rent,
      security-deposit: security-deposit,
      lease-start: lease-start-block,
      lease-end: lease-end-block,
      payment-due-date: (+ lease-start-block u4320),
      last-payment-date: u0,
      status: "active",
      lease-purpose: lease-purpose,
    }))
  )
)

(define-public (make-rent-payment
    (lease-id uint)
    (payment-amount uint)
  )
  (let (
      (lease-record (unwrap! (map-get? land-leases { lease-id: lease-id }) ERR-LEASE-NOT-FOUND))
      (current-block stacks-block-height)
      (is-late (> current-block (get payment-due-date lease-record)))
      (late-fee (if is-late
        (/ (get monthly-rent lease-record) u20)
        u0
      ))
      (total-due (+ (get monthly-rent lease-record) late-fee))
    )
    (asserts! (is-eq tx-sender (get tenant lease-record)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status lease-record) "active") ERR-LEASE-NOT-ACTIVE)
    (asserts! (>= payment-amount total-due) ERR-INSUFFICIENT-PAYMENT)
    (asserts! (<= current-block (get lease-end lease-record)) ERR-LEASE-EXPIRED)
    (var-set payment-counter (+ (var-get payment-counter) u1))
    (map-set lease-payments {
      lease-id: lease-id,
      payment-id: (var-get payment-counter),
    } {
      amount: payment-amount,
      payment-date: current-block,
      payment-type: "rent",
      late-fee: late-fee,
    })
    (ok (map-set land-leases { lease-id: lease-id }
      (merge lease-record {
        last-payment-date: current-block,
        payment-due-date: (+ current-block u4320),
      })
    ))
  )
)

(define-read-only (get-lease-details (lease-id uint))
  (ok (unwrap! (map-get? land-leases { lease-id: lease-id }) ERR-LEASE-NOT-FOUND))
)

(define-read-only (get-payment-history
    (lease-id uint)
    (payment-id uint)
  )
  (ok (map-get? lease-payments {
    lease-id: lease-id,
    payment-id: payment-id,
  }))
)

(define-read-only (is-lease-active (lease-id uint))
  (match (map-get? land-leases { lease-id: lease-id })
    lease-data (ok (and
      (is-eq (get status lease-data) "active")
      (<= stacks-block-height (get lease-end lease-data))
    ))
    (ok false)
  )
)

(define-read-only (get-lease-count)
  (ok (var-get lease-counter))
)

(define-constant ERR-PERMIT-EXISTS (err u116))
(define-constant ERR-PERMIT-NOT-FOUND (err u117))
(define-constant ERR-PERMIT-EXPIRED (err u118))
(define-constant ERR-INVALID-PERMIT-TYPE (err u119))
(define-constant ERR-UNAUTHORIZED-ISSUER (err u120))

(define-map development-permits
  { permit-id: uint }
  {
    land-id: uint,
    applicant: principal,
    permit-type: (string-ascii 30),
    development-description: (string-ascii 200),
    issue-date: uint,
    expiry-date: uint,
    issuing-authority: principal,
    status: (string-ascii 15),
    conditions: (string-ascii 300),
    compliance-verified: bool,
  }
)

(define-map permit-authorities
  { authority: principal }
  {
    authorized: bool,
    jurisdiction: (string-ascii 50),
    permit-types: (list 5 (string-ascii 30)),
  }
)

(define-map permit-inspections
  {
    permit-id: uint,
    inspection-id: uint,
  }
  {
    inspector: principal,
    inspection-date: uint,
    inspection-type: (string-ascii 30),
    result: (string-ascii 15),
    notes: (string-ascii 200),
  }
)

(define-data-var permit-counter uint u0)
(define-data-var inspection-counter uint u0)

(define-public (authorize-permit-authority
    (authority principal)
    (jurisdiction (string-ascii 50))
    (permit-types (list 5 (string-ascii 30)))
  )
  (begin
    (asserts! (is-eq tx-sender (var-get registry-admin)) ERR-NOT-AUTHORIZED)
    (ok (map-set permit-authorities { authority: authority } {
      authorized: true,
      jurisdiction: jurisdiction,
      permit-types: permit-types,
    }))
  )
)

(define-public (apply-for-permit
    (land-id uint)
    (permit-type (string-ascii 30))
    (development-description (string-ascii 200))
    (validity-blocks uint)
  )
  (let (
      (land-record (unwrap! (map-get? land-records { land-id: land-id }) ERR-NOT-FOUND))
      (issue-date stacks-block-height)
      (expiry-date (+ stacks-block-height validity-blocks))
    )
    (asserts! (is-eq tx-sender (get owner land-record)) ERR-NOT-AUTHORIZED)
    (var-set permit-counter (+ (var-get permit-counter) u1))
    (ok (map-set development-permits { permit-id: (var-get permit-counter) } {
      land-id: land-id,
      applicant: tx-sender,
      permit-type: permit-type,
      development-description: development-description,
      issue-date: issue-date,
      expiry-date: expiry-date,
      issuing-authority: (var-get registry-admin),
      status: "pending",
      conditions: "",
      compliance-verified: false,
    }))
  )
)

(define-public (approve-permit
    (permit-id uint)
    (conditions (string-ascii 300))
  )
  (let (
      (permit-record (unwrap! (map-get? development-permits { permit-id: permit-id })
        ERR-PERMIT-NOT-FOUND
      ))
      (authority-record (unwrap! (map-get? permit-authorities { authority: tx-sender })
        ERR-UNAUTHORIZED-ISSUER
      ))
    )
    (asserts! (get authorized authority-record) ERR-UNAUTHORIZED-ISSUER)
    (asserts! (is-eq (get status permit-record) "pending") ERR-INVALID-STATUS)
    (ok (map-set development-permits { permit-id: permit-id }
      (merge permit-record {
        status: "approved",
        conditions: conditions,
        issuing-authority: tx-sender,
      })
    ))
  )
)

(define-public (reject-permit
    (permit-id uint)
    (rejection-reason (string-ascii 300))
  )
  (let (
      (permit-record (unwrap! (map-get? development-permits { permit-id: permit-id })
        ERR-PERMIT-NOT-FOUND
      ))
      (authority-record (unwrap! (map-get? permit-authorities { authority: tx-sender })
        ERR-UNAUTHORIZED-ISSUER
      ))
    )
    (asserts! (get authorized authority-record) ERR-UNAUTHORIZED-ISSUER)
    (asserts! (is-eq (get status permit-record) "pending") ERR-INVALID-STATUS)
    (ok (map-set development-permits { permit-id: permit-id }
      (merge permit-record {
        status: "rejected",
        conditions: rejection-reason,
      })
    ))
  )
)

(define-public (conduct-inspection
    (permit-id uint)
    (inspection-type (string-ascii 30))
    (result (string-ascii 15))
    (notes (string-ascii 200))
  )
  (let (
      (permit-record (unwrap! (map-get? development-permits { permit-id: permit-id })
        ERR-PERMIT-NOT-FOUND
      ))
      (authority-record (unwrap! (map-get? permit-authorities { authority: tx-sender })
        ERR-UNAUTHORIZED-ISSUER
      ))
    )
    (asserts! (get authorized authority-record) ERR-UNAUTHORIZED-ISSUER)
    (asserts! (is-eq (get status permit-record) "approved") ERR-INVALID-STATUS)
    (var-set inspection-counter (+ (var-get inspection-counter) u1))
    (map-set permit-inspections {
      permit-id: permit-id,
      inspection-id: (var-get inspection-counter),
    } {
      inspector: tx-sender,
      inspection-date: stacks-block-height,
      inspection-type: inspection-type,
      result: result,
      notes: notes,
    })
    (if (is-eq result "passed")
      (ok (map-set development-permits { permit-id: permit-id }
        (merge permit-record { compliance-verified: true })
      ))
      (ok true)
    )
  )
)

(define-public (revoke-permit (permit-id uint))
  (let (
      (permit-record (unwrap! (map-get? development-permits { permit-id: permit-id })
        ERR-PERMIT-NOT-FOUND
      ))
      (authority-record (unwrap! (map-get? permit-authorities { authority: tx-sender })
        ERR-UNAUTHORIZED-ISSUER
      ))
    )
    (asserts! (get authorized authority-record) ERR-UNAUTHORIZED-ISSUER)
    (ok (map-set development-permits { permit-id: permit-id }
      (merge permit-record { status: "revoked" })
    ))
  )
)

(define-read-only (get-permit-details (permit-id uint))
  (ok (unwrap! (map-get? development-permits { permit-id: permit-id })
    ERR-PERMIT-NOT-FOUND
  ))
)

(define-read-only (get-inspection-details
    (permit-id uint)
    (inspection-id uint)
  )
  (ok (map-get? permit-inspections {
    permit-id: permit-id,
    inspection-id: inspection-id,
  }))
)

(define-read-only (is-permit-valid (permit-id uint))
  (match (map-get? development-permits { permit-id: permit-id })
    permit-data (ok (and
      (is-eq (get status permit-data) "approved")
      (<= stacks-block-height (get expiry-date permit-data))
    ))
    (ok false)
  )
)

(define-read-only (is-authorized-permit-authority (authority principal))
  (ok (map-get? permit-authorities { authority: authority }))
)

(define-constant ERR-MORTGAGE-EXISTS (err u121))
(define-constant ERR-MORTGAGE-NOT-FOUND (err u122))
(define-constant ERR-MORTGAGE-PAID (err u123))
(define-constant ERR-INSUFFICIENT-COLLATERAL (err u124))
(define-constant ERR-UNAUTHORIZED-LENDER (err u125))

(define-map land-mortgages
  { mortgage-id: uint }
  {
    land-id: uint,
    borrower: principal,
    lender: principal,
    principal-amount: uint,
    interest-rate: uint,
    term-blocks: uint,
    monthly-payment: uint,
    start-date: uint,
    end-date: uint,
    outstanding-balance: uint,
    payments-made: uint,
    status: (string-ascii 15),
    collateral-ratio: uint,
  }
)

(define-map mortgage-payments
  {
    mortgage-id: uint,
    payment-id: uint,
  }
  {
    amount: uint,
    principal-portion: uint,
    interest-portion: uint,
    payment-date: uint,
    late-fee: uint,
    remaining-balance: uint,
  }
)

(define-map authorized-lenders
  { lender: principal }
  {
    authorized: bool,
    max-loan-amount: uint,
    minimum-collateral-ratio: uint,
  }
)

(define-data-var mortgage-counter uint u0)
(define-data-var mortgage-payment-counter uint u0)

(define-public (authorize-lender
    (lender principal)
    (max-loan-amount uint)
    (minimum-collateral-ratio uint)
  )
  (begin
    (asserts! (is-eq tx-sender (var-get registry-admin)) ERR-NOT-AUTHORIZED)
    (ok (map-set authorized-lenders { lender: lender } {
      authorized: true,
      max-loan-amount: max-loan-amount,
      minimum-collateral-ratio: minimum-collateral-ratio,
    }))
  )
)

(define-public (originate-mortgage
    (land-id uint)
    (borrower principal)
    (principal-amount uint)
    (interest-rate uint)
    (term-blocks uint)
    (collateral-ratio uint)
  )
  (let (
      (land-record (unwrap! (map-get? land-records { land-id: land-id }) ERR-NOT-FOUND))
      (lender-record (unwrap! (map-get? authorized-lenders { lender: tx-sender })
        ERR-UNAUTHORIZED-LENDER
      ))
      (monthly-payment (/ (* principal-amount (+ u100 interest-rate)) (* term-blocks u100)))
      (start-date stacks-block-height)
      (end-date (+ stacks-block-height term-blocks))
    )
    (asserts! (get authorized lender-record) ERR-UNAUTHORIZED-LENDER)
    (asserts! (is-eq borrower (get owner land-record)) ERR-NOT-AUTHORIZED)
    (asserts! (<= principal-amount (get max-loan-amount lender-record))
      ERR-INSUFFICIENT-COLLATERAL
    )
    (asserts! (>= collateral-ratio (get minimum-collateral-ratio lender-record))
      ERR-INSUFFICIENT-COLLATERAL
    )
    (var-set mortgage-counter (+ (var-get mortgage-counter) u1))
    (ok (map-set land-mortgages { mortgage-id: (var-get mortgage-counter) } {
      land-id: land-id,
      borrower: borrower,
      lender: tx-sender,
      principal-amount: principal-amount,
      interest-rate: interest-rate,
      term-blocks: term-blocks,
      monthly-payment: monthly-payment,
      start-date: start-date,
      end-date: end-date,
      outstanding-balance: principal-amount,
      payments-made: u0,
      status: "active",
      collateral-ratio: collateral-ratio,
    }))
  )
)

(define-public (make-mortgage-payment
    (mortgage-id uint)
    (payment-amount uint)
  )
  (let (
      (mortgage-record (unwrap! (map-get? land-mortgages { mortgage-id: mortgage-id })
        ERR-MORTGAGE-NOT-FOUND
      ))
      (monthly-payment (get monthly-payment mortgage-record))
      (outstanding-balance (get outstanding-balance mortgage-record))
      (interest-portion (/ (* outstanding-balance (get interest-rate mortgage-record)) u1200))
      (principal-portion (- payment-amount interest-portion))
      (new-balance (- outstanding-balance principal-portion))
      (is-late (> stacks-block-height
        (+ (get start-date mortgage-record)
          (* (+ (get payments-made mortgage-record) u1) u4320)
        )))
      (late-fee (if is-late
        (/ monthly-payment u20)
        u0
      ))
      (total-due (+ monthly-payment late-fee))
    )
    (asserts! (is-eq tx-sender (get borrower mortgage-record)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status mortgage-record) "active") ERR-INVALID-STATUS)
    (asserts! (>= payment-amount total-due) ERR-INSUFFICIENT-PAYMENT)
    (asserts! (<= stacks-block-height (get end-date mortgage-record))
      ERR-LEASE-EXPIRED
    )
    (var-set mortgage-payment-counter (+ (var-get mortgage-payment-counter) u1))
    (map-set mortgage-payments {
      mortgage-id: mortgage-id,
      payment-id: (var-get mortgage-payment-counter),
    } {
      amount: payment-amount,
      principal-portion: principal-portion,
      interest-portion: interest-portion,
      payment-date: stacks-block-height,
      late-fee: late-fee,
      remaining-balance: new-balance,
    })
    (if (<= new-balance u0)
      (ok (map-set land-mortgages { mortgage-id: mortgage-id }
        (merge mortgage-record {
          outstanding-balance: u0,
          payments-made: (+ (get payments-made mortgage-record) u1),
          status: "paid",
        })
      ))
      (ok (map-set land-mortgages { mortgage-id: mortgage-id }
        (merge mortgage-record {
          outstanding-balance: new-balance,
          payments-made: (+ (get payments-made mortgage-record) u1),
        })
      ))
    )
  )
)

(define-public (initiate-foreclosure (mortgage-id uint))
  (let (
      (mortgage-record (unwrap! (map-get? land-mortgages { mortgage-id: mortgage-id })
        ERR-MORTGAGE-NOT-FOUND
      ))
      (expected-payments (/ (- stacks-block-height (get start-date mortgage-record)) u4320))
      (missed-payments (- expected-payments (get payments-made mortgage-record)))
    )
    (asserts! (is-eq tx-sender (get lender mortgage-record)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status mortgage-record) "active") ERR-INVALID-STATUS)
    (asserts! (>= missed-payments u3) ERR-INVALID-STATUS)
    (ok (map-set land-mortgages { mortgage-id: mortgage-id }
      (merge mortgage-record { status: "foreclosure" })
    ))
  )
)

(define-public (complete-foreclosure (mortgage-id uint))
  (let (
      (mortgage-record (unwrap! (map-get? land-mortgages { mortgage-id: mortgage-id })
        ERR-MORTGAGE-NOT-FOUND
      ))
      (land-record (unwrap! (map-get? land-records { land-id: (get land-id mortgage-record) })
        ERR-NOT-FOUND
      ))
    )
    (asserts! (is-eq tx-sender (get lender mortgage-record)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status mortgage-record) "foreclosure")
      ERR-INVALID-STATUS
    )
    (map-set land-records { land-id: (get land-id mortgage-record) }
      (merge land-record { owner: (get lender mortgage-record) })
    )
    (ok (map-set land-mortgages { mortgage-id: mortgage-id }
      (merge mortgage-record { status: "foreclosed" })
    ))
  )
)

(define-read-only (get-mortgage-details (mortgage-id uint))
  (ok (unwrap! (map-get? land-mortgages { mortgage-id: mortgage-id })
    ERR-MORTGAGE-NOT-FOUND
  ))
)

(define-read-only (get-mortgage-payment-details
    (mortgage-id uint)
    (payment-id uint)
  )
  (ok (map-get? mortgage-payments {
    mortgage-id: mortgage-id,
    payment-id: payment-id,
  }))
)

(define-read-only (is-authorized-lender (lender principal))
  (match (map-get? authorized-lenders { lender: lender })
    lender-data (ok (get authorized lender-data))
    (ok false)
  )
)

(define-read-only (get-mortgage-count)
  (ok (var-get mortgage-counter))
)

(define-read-only (calculate-mortgage-health (mortgage-id uint))
  (let (
      (mortgage-record (unwrap! (map-get? land-mortgages { mortgage-id: mortgage-id })
        ERR-MORTGAGE-NOT-FOUND
      ))
      (expected-payments (/ (- stacks-block-height (get start-date mortgage-record)) u4320))
      (payment-ratio (if (> expected-payments u0)
        (/ (* (get payments-made mortgage-record) u100) expected-payments)
        u100
      ))
    )
    (ok {
      payment-compliance: payment-ratio,
      outstanding-balance: (get outstanding-balance mortgage-record),
      status: (get status mortgage-record),
    })
  )
)

(define-constant ERR-NO-ANALYTICS-DATA (err u126))

(define-map land-analytics
  { land-id: uint }
  {
    transfer-count: uint,
    last-valuation: uint,
    dispute-count: uint,
    mortgage-status: (string-ascii 15),
    performance-score: uint,
  }
)

(define-public (update-land-analytics (land-id uint))
  (let (
      (land-record (unwrap! (map-get? land-records { land-id: land-id }) ERR-NOT-FOUND))
      (has-transfer (is-some (map-get? land-history {
        land-id: land-id,
        transaction-id: (var-get transaction-counter),
      })))
      (latest-valuation (match (map-get? land-valuations {
        land-id: land-id,
        valuation-id: (var-get valuation-counter),
      })
        val-record (get market-value val-record)
        u0
      ))
      (has-mortgage (is-some (map-get? land-mortgages { mortgage-id: (var-get mortgage-counter) })))
      (mortgage-status (if has-mortgage
        "mortgaged"
        "clear"
      ))
      (performance (if (and has-transfer (> latest-valuation u0))
        u85
        u60
      ))
    )
    (ok (map-set land-analytics { land-id: land-id } {
      transfer-count: (if has-transfer
        u1
        u0
      ),
      last-valuation: latest-valuation,
      dispute-count: u0,
      mortgage-status: mortgage-status,
      performance-score: performance,
    }))
  )
)

(define-read-only (get-land-analytics (land-id uint))
  (ok (map-get? land-analytics { land-id: land-id }))
)

(define-read-only (calculate-market-summary)
  (let (
      (total-transfers (var-get transaction-counter))
      (total-disputes (var-get dispute-counter))
      (total-mortgages (var-get mortgage-counter))
      (total-valuations (var-get valuation-counter))
      (market-health (if (> total-transfers total-disputes)
        u80
        u50
      ))
    )
    (ok {
      total-transfers: total-transfers,
      total-disputes: total-disputes,
      total-mortgages: total-mortgages,
      total-valuations: total-valuations,
      market-health-score: market-health,
      snapshot-date: stacks-block-height,
    })
  )
)

(define-read-only (get-land-performance (land-id uint))
  (match (map-get? land-analytics { land-id: land-id })
    analytics-data (match (map-get? land-records { land-id: land-id })
      land-data (ok {
        land-id: land-id,
        performance-score: (get performance-score analytics-data),
        transfer-activity: (get transfer-count analytics-data),
        current-valuation: (get last-valuation analytics-data),
        land-status: (get status land-data),
      })
      ERR-NOT-FOUND
    )
    ERR-NO-ANALYTICS-DATA
  )
)

;; Land Audit Trail System
;; Provides comprehensive audit logging for all administrative actions

(define-constant ERR-AUDIT-NOT-FOUND (err u127))
(define-constant ERR-INVALID-AUDIT-TYPE (err u128))

;; Main audit trail map storing all system actions
(define-map audit-trail
  { audit-id: uint }
  {
    action-type: (string-ascii 30),
    entity-type: (string-ascii 20),
    entity-id: uint,
    actor: principal,
    timestamp: uint,
    action-details: (string-ascii 200),
    previous-value: (optional (string-ascii 100)),
    new-value: (optional (string-ascii 100)),
    affected-parties: (list 3 principal),
  }
)

;; Audit categories for filtering and reporting
(define-map audit-categories
  { category: (string-ascii 30) }
  {
    description: (string-ascii 100),
    retention-blocks: uint,
    compliance-required: bool,
  }
)

;; Audit summary for quick statistics
(define-map audit-summary
  { summary-date: uint }
  {
    total-actions: uint,
    admin-actions: uint,
    transfer-actions: uint,
    dispute-actions: uint,
    financial-actions: uint,
    compliance-score: uint,
  }
)

(define-data-var audit-counter uint u0)
(define-data-var last-summary-date uint u0)

;; Initialize audit categories
(map-set audit-categories { category: "land-registration" } {
  description: "Land registration and ownership changes",
  retention-blocks: u525600,  ;; ~1 year
  compliance-required: true,
})

(map-set audit-categories { category: "financial-transaction" } {
  description: "Mortgages, payments, and valuations",
  retention-blocks: u2102400, ;; ~4 years
  compliance-required: true,
})

(map-set audit-categories { category: "administrative" } {
  description: "Admin actions and system changes",
  retention-blocks: u1051200, ;; ~2 years
  compliance-required: true,
})

(map-set audit-categories { category: "dispute-resolution" } {
  description: "Disputes and resolution processes",
  retention-blocks: u1576800, ;; ~3 years
  compliance-required: true,
})

;; Log audit trail entry
(define-public (log-audit-entry
    (action-type (string-ascii 30))
    (entity-type (string-ascii 20))
    (entity-id uint)
    (action-details (string-ascii 200))
    (previous-value (optional (string-ascii 100)))
    (new-value (optional (string-ascii 100)))
    (affected-parties (list 3 principal))
  )
  (begin
    (var-set audit-counter (+ (var-get audit-counter) u1))
    (ok (map-set audit-trail { audit-id: (var-get audit-counter) } {
      action-type: action-type,
      entity-type: entity-type,
      entity-id: entity-id,
      actor: tx-sender,
      timestamp: stacks-block-height,
      action-details: action-details,
      previous-value: previous-value,
      new-value: new-value,
      affected-parties: affected-parties,
    }))
  )
)

;; Log land registration audit
(define-public (log-land-registration-audit
    (land-id uint)
    (owner principal)
    (area uint)
    (land-type (string-ascii 20))
  )
  (let ((area-str (int-to-ascii area)))
    (log-audit-entry
      "land-registration"
      "land"
      land-id
      "New land registered in system"
      none
      (some land-type)
      (list owner)
    )
  )
)

;; Log land transfer audit
(define-public (log-land-transfer-audit
    (land-id uint)
    (previous-owner principal)
    (new-owner principal)
  )
  (log-audit-entry
    "land-transfer"
    "land"
    land-id
    "Land ownership transferred"
    (some "ownership-change")
    (some "transferred")
    (list previous-owner new-owner)
  )
)

;; Log administrative action audit
(define-public (log-admin-action-audit
    (action-type (string-ascii 30))
    (target-entity (string-ascii 20))
    (entity-id uint)
    (details (string-ascii 200))
  )
  (begin
    (asserts! (is-eq tx-sender (var-get registry-admin)) ERR-NOT-AUTHORIZED)
    (log-audit-entry
      action-type
      target-entity
      entity-id
      details
      none
      none
      (list (var-get registry-admin))
    )
  )
)

;; Log dispute action audit
(define-public (log-dispute-audit
    (dispute-id uint)
    (action (string-ascii 30))
    (complainant principal)
    (respondent principal)
    (details (string-ascii 200))
  )
  (log-audit-entry
    "dispute-action"
    "dispute"
    dispute-id
    details
    none
    (some action)
    (list complainant respondent)
  )
)

;; Log financial transaction audit
(define-public (log-financial-audit
    (transaction-type (string-ascii 30))
    (entity-id uint)
    (amount uint)
    (parties (list 3 principal))
    (details (string-ascii 200))
  )
  (let ((amount-str (int-to-ascii amount)))
    (log-audit-entry
      transaction-type
      "financial"
      entity-id
      details
      none
      (some "completed")
      parties
    )
  )
)

;; Generate daily audit summary
(define-public (generate-audit-summary)
  (let (
      (current-date stacks-block-height)
      (total-audits (var-get audit-counter))
      (admin-actions u0) ;; Simplified for this implementation
      (transfer-actions u0)
      (dispute-actions u0)
      (financial-actions u0)
      (compliance-score (if (> total-audits u0) u95 u0))
    )
    (asserts! (is-eq tx-sender (var-get registry-admin)) ERR-NOT-AUTHORIZED)
    (var-set last-summary-date current-date)
    (ok (map-set audit-summary { summary-date: current-date } {
      total-actions: total-audits,
      admin-actions: admin-actions,
      transfer-actions: transfer-actions,
      dispute-actions: dispute-actions,
      financial-actions: financial-actions,
      compliance-score: compliance-score,
    }))
  )
)

;; Get audit entry by ID
(define-read-only (get-audit-entry (audit-id uint))
  (ok (unwrap! (map-get? audit-trail { audit-id: audit-id })
    ERR-AUDIT-NOT-FOUND
  ))
)

;; Get audit entries for specific entity
(define-read-only (get-entity-audit-trail
    (entity-type (string-ascii 20))
    (entity-id uint)
  )
  ;; Simplified implementation - in production, this would filter results
  (ok (list
    { audit-id: u1, found: true }
  ))
)

;; Get audit trail for a specific land parcel
(define-read-only (get-land-audit-trail (land-id uint))
  (get-entity-audit-trail "land" land-id)
)

;; Get audit summary for a date
(define-read-only (get-audit-summary (summary-date uint))
  (ok (map-get? audit-summary { summary-date: summary-date }))
)

;; Get latest audit summary
(define-read-only (get-latest-audit-summary)
  (get-audit-summary (var-get last-summary-date))
)

;; Check audit category compliance
(define-read-only (get-audit-category (category (string-ascii 30)))
  (ok (map-get? audit-categories { category: category }))
)

;; Verify audit integrity (simplified)
(define-read-only (verify-audit-integrity (start-audit-id uint) (end-audit-id uint))
  (let (
      (start-exists (is-some (map-get? audit-trail { audit-id: start-audit-id })))
      (end-exists (is-some (map-get? audit-trail { audit-id: end-audit-id })))
      (range-valid (<= start-audit-id end-audit-id))
    )
    (ok {
      range-valid: range-valid,
      start-exists: start-exists,
      end-exists: end-exists,
      integrity-score: (if (and range-valid start-exists end-exists) u100 u0),
    })
  )
)

;; Get audit statistics
(define-read-only (get-audit-statistics)
  (ok {
    total-audit-entries: (var-get audit-counter),
    last-summary-date: (var-get last-summary-date),
    current-block: stacks-block-height,
    audit-categories: u4,
  })
)

;; Check if action requires audit logging
(define-read-only (is-audit-required (action-type (string-ascii 30)))
  (let (
      (is-admin-action (or
        (is-eq action-type "admin-change")
        (is-eq action-type "authorization")
      ))
      (is-financial (or
        (is-eq action-type "mortgage")
        (is-eq action-type "payment")
        (is-eq action-type "valuation")
      ))
      (is-land-action (or
        (is-eq action-type "registration")
        (is-eq action-type "transfer")
      ))
    )
    (ok (or is-admin-action (or is-financial is-land-action)))
  )
)
