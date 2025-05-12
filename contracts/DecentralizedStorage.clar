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

(define-map storage-listings
  principal
  {
    space-amount: uint,
    price: uint,
    expiry: uint,
    is-active: bool
  }
)

(define-public (create-storage-listing (space uint) (price uint) (duration uint))
  (let ((provider tx-sender))
    (asserts! (is-some (get-provider-details provider)) err-not-registered)
    (map-set storage-listings provider
      {
        space-amount: space,
        price: price,
        expiry: (+ stacks-block-height duration),
        is-active: true
      }
    )
    (ok true)
  )
)

(define-public (purchase-storage-listing (provider principal))
  (let (
    (listing (unwrap! (map-get? storage-listings provider) err-provider-not-found))
    (buyer tx-sender)
  )
    (asserts! (get is-active listing) err-provider-not-found)
    (asserts! (<= stacks-block-height (get expiry listing)) err-invalid-amount)
    (request-storage provider (get space-amount listing))
  )
)


(define-data-var payment-cycle uint u144) ;; ~1 day in blocks

(define-map payment-records
  { user: principal, provider: principal }
  { last-paid: uint, amount: uint }
)

(define-public (process-payment (provider principal))
  (let (
    (user tx-sender)
    (agreement (unwrap! (get-agreement-details user provider) err-not-registered))
    (payment-amount (* (get space-allocated agreement) (get price-per-gb agreement)))
  )
    (asserts! (>= (- stacks-block-height (get last-payment (unwrap! (get-user-details user) err-not-registered))) (var-get payment-cycle)) err-invalid-amount)
    (map-set payment-records {user: user, provider: provider}
      { last-paid: stacks-block-height, amount: payment-amount }
    )
    (ok payment-amount)
  )
)



(define-map storage-usage
  principal
  {
    used-space: uint,
    last-updated: uint,
    usage-history: (list 10 uint)
  }
)

(define-public (update-storage-usage (user principal) (space-used uint))
  (let (
    (provider tx-sender)
    (current-usage (default-to 
      {used-space: u0, last-updated: u0, usage-history: (list)} 
      (map-get? storage-usage user)
    ))
  )
    (asserts! (is-some (get-agreement-details user provider)) err-not-registered)
    (map-set storage-usage user
      {
        used-space: space-used,
        last-updated: stacks-block-height,
        usage-history: (unwrap-panic (as-max-len? 
          (append (get usage-history current-usage) space-used) 
          u10
        ))
      }
    )
    (ok true)
  )
)


(define-map qos-metrics
  principal
  {
    availability: uint,
    latency: uint,
    bandwidth: uint,
    last-check: uint
  }
)

(define-public (update-qos-metrics (provider principal) (availability uint) (latency uint) (bandwidth uint))
  (let ((caller tx-sender))
    (asserts! (is-eq caller contract-owner) err-owner-only)
    (map-set qos-metrics provider
      {
        availability: availability,
        latency: latency,
        bandwidth: bandwidth,
        last-check: stacks-block-height
      }
    )
    (ok true)
  )
)


(define-map node-health
  principal
  {
    is-online: bool,
    last-ping: uint,
    consecutive-failures: uint,
    warnings: (list 5 (string-ascii 50))
  }
)

(define-public (report-node-status (is-online bool))
  (let (
    (provider tx-sender)
    (current-health (default-to 
      {is-online: false, last-ping: u0, consecutive-failures: u0, warnings: (list)}
      (map-get? node-health provider)
    ))
  )
    (asserts! (is-some (get-provider-details provider)) err-not-registered)
    (map-set node-health provider
      {
        is-online: is-online,
        last-ping: stacks-block-height,
        consecutive-failures: (if is-online u0 (+ (get consecutive-failures current-health) u1)),
        warnings: (get warnings current-health)
      }
    )
    (ok true)
  )
)


(define-constant err-invalid-key (err u106))
(define-constant err-unauthorized (err u107))

(define-map encryption-keys
  { user: principal, provider: principal }
  {
    public-key: (string-ascii 64),
    key-version: uint,
    created-at: uint,
    last-rotated: uint
  }
)

(define-public (register-encryption-key (provider principal) (public-key (string-ascii 64)))
  (let ((user tx-sender))
    (asserts! (is-some (get-agreement-details user provider)) err-not-registered)
    (map-set encryption-keys {user: user, provider: provider}
      {
        public-key: public-key,
        key-version: u1,
        created-at: stacks-block-height,
        last-rotated: stacks-block-height
      }
    )
    (ok true)
  )
)


(define-public (rotate-encryption-key (provider principal) (new-key (string-ascii 64)))
  (let ((user tx-sender))
    (asserts! (is-some (get-agreement-details user provider)) err-not-registered)
    (let ((key-details (unwrap! (map-get? encryption-keys {user: user, provider: provider}) err-invalid-key)))
      (map-set encryption-keys {user: user, provider: provider}
        {
          public-key: new-key,
          key-version: (+ (get key-version key-details) u1),
          created-at: stacks-block-height,
          last-rotated: stacks-block-height
        }
      )
    )
    (ok true)
  )
)
(define-public (get-encryption-key (provider principal))
  (let ((user tx-sender))
    (asserts! (is-some (get-agreement-details user provider)) err-not-registered)
    (ok (unwrap! (map-get? encryption-keys {user: user, provider: provider}) err-invalid-key))
  )
)
(define-public (get-encryption-key-version (provider principal))
  (let ((user tx-sender))
    (asserts! (is-some (get-agreement-details user provider)) err-not-registered)
    (ok (get key-version (unwrap! (map-get? encryption-keys {user: user, provider: provider}) err-invalid-key)))
  )
)
(define-public (get-encryption-key-last-rotated (provider principal))
  (let ((user tx-sender))
    (asserts! (is-some (get-agreement-details user provider)) err-not-registered)
    (ok (get last-rotated (unwrap! (map-get? encryption-keys {user: user, provider: provider}) err-invalid-key)))
  )
)
(define-public (get-encryption-key-created-at (provider principal))
  (let ((user tx-sender))
    (asserts! (is-some (get-agreement-details user provider)) err-not-registered)
    (ok (get created-at (unwrap! (map-get? encryption-keys {user: user, provider: provider}) err-invalid-key)))
  )
)
(define-public (get-encryption-key-public-key (provider principal))
  (let ((user tx-sender))
    (asserts! (is-some (get-agreement-details user provider)) err-not-registered)
    (ok (get public-key (unwrap! (map-get? encryption-keys {user: user, provider: provider}) err-invalid-key)))
  )
)
(define-public (get-encryption-key-user (provider principal))
  (let ((user tx-sender))
    (asserts! (is-some (get-agreement-details user provider)) err-not-registered)
    (ok user)
  )
)
(define-public (get-encryption-key-provider (provider principal))
  (let ((user tx-sender))
    (asserts! (is-some (get-agreement-details user provider)) err-not-registered)
    (ok provider)
  )
)
(define-public (get-encryption-key-user-provider (provider principal))
  (let ((user tx-sender))
    (asserts! (is-some (get-agreement-details user provider)) err-not-registered)
    (ok {user: user, provider: provider})
  )
)

(define-map provider-reputation
    principal
    {
        rating-sum: uint,
        rating-count: uint,
        weighted-score: uint
    }
)

(define-public (rate-provider (provider principal) (rating uint))
    (let (
        (user tx-sender)
        (current-rep (default-to {rating-sum: u0, rating-count: u0, weighted-score: u0} 
            (map-get? provider-reputation provider)))
        (provider-qos (unwrap! (map-get? qos-metrics provider) err-provider-not-found))
        (uptime-weight u3)
        (qos-weight u2) 
        (rating-weight u5)
    )
        (asserts! (and (>= rating u0) (<= rating u100)) err-invalid-amount)
        (asserts! (is-some (get-agreement-details user provider)) err-not-registered)
        
        (let ((new-rating-sum (+ (get rating-sum current-rep) rating))
              (new-rating-count (+ (get rating-count current-rep) u1))
              (avg-rating (/ new-rating-sum new-rating-count))
              (weighted-score (/ (+ 
                (* (get uptime-score (unwrap! (get-provider-details provider) err-provider-not-found)) uptime-weight)
                (* (get availability provider-qos) qos-weight)
                (* avg-rating rating-weight)
              ) (+ uptime-weight qos-weight rating-weight))))
            
            (map-set provider-reputation provider
                {
                    rating-sum: new-rating-sum,
                    rating-count: new-rating-count,
                    weighted-score: weighted-score
                }
            )
            (ok weighted-score)
        )
    )
)


(define-map storage-trades
    uint
    {
        seller: principal,
        provider: principal,
        space-amount: uint,
        price: uint,
        is-active: bool
    }
)

(define-data-var trade-nonce uint u0)

(define-public (create-storage-trade (provider principal) (space uint) (price uint))
    (let (
        (seller tx-sender)
        (agreement (unwrap! (get-agreement-details seller provider) err-not-registered))
        (trade-id (var-get trade-nonce))
    )
        (asserts! (<= space (get space-allocated agreement)) err-invalid-amount)
        
        (map-set storage-trades trade-id
            {
                seller: seller,
                provider: provider,
                space-amount: space,
                price: price,
                is-active: true
            }
        )
        
        (var-set trade-nonce (+ trade-id u1))
        (ok trade-id)
    )
)

(define-public (execute-storage-trade (trade-id uint))
    (let (
        (buyer tx-sender)
        (trade (unwrap! (map-get? storage-trades trade-id) err-not-registered))
    )
        (asserts! (get is-active trade) err-invalid-amount)
        (asserts! (not (is-eq buyer (get seller trade))) err-unauthorized)
        
        (try! (request-storage (get provider trade) (get space-amount trade)))
        
        (map-set storage-trades trade-id
            (merge trade {is-active: false})
        )
        (ok true)
    )
)