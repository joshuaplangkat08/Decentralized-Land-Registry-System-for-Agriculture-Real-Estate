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

(define-public (terminate-lease (lease-id uint))
  (let ((lease-record (unwrap! (map-get? land-leases { lease-id: lease-id }) ERR-LEASE-NOT-FOUND)))
    (asserts!
      (or
        (is-eq tx-sender (get landlord lease-record))
        (is-eq tx-sender (get tenant lease-record))
      )
      ERR-NOT-AUTHORIZED
    )
    (ok (map-set land-leases { lease-id: lease-id }
      (merge lease-record { status: "terminated" })
    ))
  )
)

(define-public (extend-lease
    (lease-id uint)
    (additional-blocks uint)
  )
  (let ((lease-record (unwrap! (map-get? land-leases { lease-id: lease-id }) ERR-LEASE-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get landlord lease-record)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status lease-record) "active") ERR-LEASE-NOT-ACTIVE)
    (ok (map-set land-leases { lease-id: lease-id }
      (merge lease-record { lease-end: (+ (get lease-end lease-record) additional-blocks) })
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
