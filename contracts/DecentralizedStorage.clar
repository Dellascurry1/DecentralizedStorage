;; DecentralizedStorage - A distributed storage marketplace
;; Users can stake tokens to reserve storage space and providers can offer storage

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-already-registered (err u101))
(define-constant err-not-registered (err u102))
(define-constant err-insufficient-stake (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-provider-not-found (err u105))
(define-constant minimum-stake u1000)

;; Data Variables
(define-data-var total-storage-providers uint u0)
(define-data-var total-users uint u0)
(define-data-var total-storage-allocated uint u0)

;; Data Maps
(define-map storage-providers principal 
  {
    available-space: uint,
    price-per-gb: uint,
    total-stake: uint,
    uptime-score: uint,
    is-active: bool
  }
)

(define-map users principal
  {
    allocated-space: uint,
    staked-amount: uint,
    provider: (optional principal),
    last-payment: uint
  }
)

(define-map storage-agreements
  {user: principal, provider: principal}
  {
    space-allocated: uint,
    price-per-gb: uint,
    start-block: uint,
    end-block: uint
  }
)

;; Public Functions

;; Register as a storage provider
(define-public (register-provider (available-space uint) (price-per-gb uint))
  (let ((provider tx-sender))
    (asserts! (is-none (get-provider-details provider)) err-already-registered)
    (asserts! (> available-space u0) err-invalid-amount)
    (asserts! (> price-per-gb u0) err-invalid-amount)
    
    (map-set storage-providers provider {
      available-space: available-space,
      price-per-gb: price-per-gb,
      total-stake: u0,
      uptime-score: u100,
      is-active: true
    })
    
    (var-set total-storage-providers (+ (var-get total-storage-providers) u1))
    (ok true)
  )
)

;; Register as a user and stake tokens
(define-public (register-user (stake-amount uint))
  (let ((user tx-sender))
    (asserts! (>= stake-amount minimum-stake) err-insufficient-stake)
    (asserts! (is-none (get-user-details user)) err-already-registered)
    
    (map-set users user {
      allocated-space: u0,
      staked-amount: stake-amount,
      provider: none,
      last-payment: stacks-block-height
    })
    
    (var-set total-users (+ (var-get total-users) u1))
    (ok true)
  )
)

;; Request storage allocation
(define-public (request-storage (provider principal) (space-needed uint))
  (let (
    (user tx-sender)
    (provider-details (unwrap! (get-provider-details provider) err-provider-not-found))
    )
    
    (asserts! (>= (get available-space provider-details) space-needed) err-invalid-amount)
    (asserts! (is-some (get-user-details user)) err-not-registered)
    
    (map-set storage-agreements {user: user, provider: provider}
      {
        space-allocated: space-needed,
        price-per-gb: (get price-per-gb provider-details),
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height u1440) ;; ~10 days in blocks
      }
    )
    
    (var-set total-storage-allocated (+ (var-get total-storage-allocated) space-needed))
    (ok true)
  )
)

;; Read-only Functions

;; Get provider details
(define-read-only (get-provider-details (provider principal))
  (map-get? storage-providers provider)
)

;; Get user details
(define-read-only (get-user-details (user principal))
  (map-get? users user)
)

;; Get agreement details
(define-read-only (get-agreement-details (user principal) (provider principal))
  (map-get? storage-agreements {user: user, provider: provider})
)

;; Get total statistics
(define-read-only (get-storage-stats)
  (ok {
    total-providers: (var-get total-storage-providers),
    total-users: (var-get total-users),
    total-storage: (var-get total-storage-allocated)
  })
)

;; Private Functions

;; Update provider uptime score
(define-private (update-uptime-score (provider principal) (new-score uint))
  (let ((provider-details (unwrap! (get-provider-details provider) err-provider-not-found)))
    (map-set storage-providers provider
      (merge provider-details {uptime-score: new-score})
    )
    (ok true)
  )
)

