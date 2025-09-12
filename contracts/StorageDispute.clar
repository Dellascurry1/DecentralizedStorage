;; StorageDispute.clar - Simple dispute resolution for storage agreements
;; Users can file disputes against providers about their storage agreements.
;; Contract owner can resolve disputes by setting outcome and remedy percentage.

(define-constant contract-owner tx-sender)

(define-constant err-not-owner (err u600))
(define-constant err-agreement-not-found (err u601))
(define-constant err-invalid-remedy (err u602))
(define-constant err-not-found (err u603))
(define-constant err-already-resolved (err u604))

(define-data-var next-dispute-id uint u0)

(define-map disputes
  uint
  {
    user: principal,
    provider: principal,
    space-allocated: uint,
    evidence-hash: (string-ascii 64),
    reason: (string-ascii 200),
    filed-at: uint,
    status: (string-ascii 20),
    remedy-percentage: uint,
    resolution-notes: (string-ascii 200),
    resolver: (optional principal),
    resolved-at: (optional uint)
  }
)

(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes dispute-id)
)

(define-read-only (get-next-dispute-id)
  (var-get next-dispute-id)
)

;; Users file disputes referencing an existing agreement in DecentralizedStorage
(define-public (file-dispute (provider principal) (evidence-hash (string-ascii 64)) (reason (string-ascii 200)))
  (let (
    (user tx-sender)
    (agreement-opt (contract-call? .DecentralizedStorage get-agreement-details user provider))
  )
    (match agreement-opt
      agreement
        (let (
          (dispute-id (var-get next-dispute-id))
        )
          (map-set disputes dispute-id
            {
              user: user,
              provider: provider,
              space-allocated: (get space-allocated agreement),
              evidence-hash: evidence-hash,
              reason: reason,
              filed-at: stacks-block-height,
              status: "open",
              remedy-percentage: u0,
              resolution-notes: "",
              resolver: none,
              resolved-at: none
            }
          )
          (var-set next-dispute-id (+ dispute-id u1))
          (ok dispute-id)
        )
      (err err-agreement-not-found)
    )
  )
)

;; Owner resolves a dispute, setting status and optional remedy percentage (0-100)
(define-public (resolve-dispute (dispute-id uint) (new-status (string-ascii 20)) (remedy-percentage uint) (notes (string-ascii 200)))
  (let (
    (caller tx-sender)
    (dispute (unwrap! (map-get? disputes dispute-id) err-not-found))
  )
    (asserts! (is-eq caller contract-owner) err-not-owner)
    (asserts! (is-eq (get status dispute) "open") err-already-resolved)
    (asserts! (and (>= remedy-percentage u0) (<= remedy-percentage u100)) err-invalid-remedy)
    (map-set disputes dispute-id
      (merge dispute {
        status: new-status,
        remedy-percentage: remedy-percentage,
        resolution-notes: notes,
        resolver: (some caller),
        resolved-at: (some stacks-block-height)
      })
    )
    (ok true)
  )
)
