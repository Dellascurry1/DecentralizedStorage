(define-constant err-policy-not-found (err u300))
(define-constant err-insufficient-coverage (err u301))
(define-constant err-claim-expired (err u302))
(define-constant err-invalid-claim (err u303))
(define-constant err-insufficient-underwriter-stake (err u304))
(define-constant err-policy-expired (err u305))
(define-constant err-claim-already-processed (err u306))
(define-constant err-unauthorized-claim (err u307))
(define-constant err-invalid-premium (err u308))
(define-constant err-insufficient-funds (err u309))

(define-constant minimum-underwriter-stake u10000)
(define-constant maximum-coverage-ratio u5)
(define-constant base-premium-rate u100)
(define-constant claim-window-blocks u1440)

(define-data-var total-policies uint u0)
(define-data-var total-claims uint u0)
(define-data-var total-underwriter-stake uint u0)
(define-data-var insurance-pool uint u0)

(define-map insurance-policies
  uint
  {
    user: principal,
    provider: principal,
    coverage-amount: uint,
    premium-paid: uint,
    start-block: uint,
    end-block: uint,
    is-active: bool
  }
)

(define-map underwriter-stakes
  principal
  {
    stake-amount: uint,
    coverage-capacity: uint,
    total-premiums-earned: uint,
    active-policies: uint,
    reputation-score: uint
  }
)

(define-map insurance-claims
  uint
  {
    policy-id: uint,
    claimant: principal,
    claim-amount: uint,
    evidence-hash: (string-ascii 64),
    filed-at: uint,
    status: (string-ascii 20),
    validator: (optional principal),
    payout-amount: uint
  }
)

(define-map policy-underwriters
  {policy-id: uint, underwriter: principal}
  {
    coverage-share: uint,
    premium-share: uint
  }
)

(define-public (register-underwriter (stake-amount uint))
  (let (
    (underwriter tx-sender)
    (coverage-capacity (* stake-amount maximum-coverage-ratio))
  )
    (asserts! (>= stake-amount minimum-underwriter-stake) err-insufficient-underwriter-stake)
    
    (map-set underwriter-stakes underwriter
      {
        stake-amount: stake-amount,
        coverage-capacity: coverage-capacity,
        total-premiums-earned: u0,
        active-policies: u0,
        reputation-score: u100
      }
    )
    
    (var-set total-underwriter-stake (+ (var-get total-underwriter-stake) stake-amount))
    (var-set insurance-pool (+ (var-get insurance-pool) stake-amount))
    (ok true)
  )
)

(define-public (create-insurance-policy (provider principal) (coverage-amount uint) (duration-blocks uint))
  (let (
    (user tx-sender)
    (policy-id (var-get total-policies))
    (premium-amount (calculate-premium coverage-amount duration-blocks))
    (end-block (+ stacks-block-height duration-blocks))
  )
    (asserts! (> coverage-amount u0) err-invalid-premium)
    (asserts! (> duration-blocks u0) err-invalid-premium)
    
    (map-set insurance-policies policy-id
      {
        user: user,
        provider: provider,
        coverage-amount: coverage-amount,
        premium-paid: premium-amount,
        start-block: stacks-block-height,
        end-block: end-block,
        is-active: true
      }
    )
    
    (var-set total-policies (+ policy-id u1))
    (var-set insurance-pool (+ (var-get insurance-pool) premium-amount))
    (ok policy-id)
  )
)

(define-public (assign-underwriter-to-policy (policy-id uint) (coverage-share uint))
  (let (
    (underwriter tx-sender)
    (policy (unwrap! (map-get? insurance-policies policy-id) err-policy-not-found))
    (underwriter-details (unwrap! (map-get? underwriter-stakes underwriter) err-insufficient-underwriter-stake))
    (required-coverage (* (get coverage-amount policy) coverage-share))
    (premium-share (* (get premium-paid policy) coverage-share))
  )
    (asserts! (get is-active policy) err-policy-expired)
    (asserts! (>= (get coverage-capacity underwriter-details) required-coverage) err-insufficient-coverage)
    (asserts! (and (> coverage-share u0) (<= coverage-share u100)) err-invalid-premium)
    
    (map-set policy-underwriters {policy-id: policy-id, underwriter: underwriter}
      {
        coverage-share: coverage-share,
        premium-share: premium-share
      }
    )
    
    (map-set underwriter-stakes underwriter
      (merge underwriter-details 
        {
          coverage-capacity: (- (get coverage-capacity underwriter-details) required-coverage),
          total-premiums-earned: (+ (get total-premiums-earned underwriter-details) premium-share),
          active-policies: (+ (get active-policies underwriter-details) u1)
        }
      )
    )
    
    (ok true)
  )
)

(define-public (file-insurance-claim (policy-id uint) (claim-amount uint) (evidence-hash (string-ascii 64)))
  (let (
    (claimant tx-sender)
    (policy (unwrap! (map-get? insurance-policies policy-id) err-policy-not-found))
    (claim-id (var-get total-claims))
  )
    (asserts! (is-eq claimant (get user policy)) err-unauthorized-claim)
    (asserts! (get is-active policy) err-policy-expired)
    (asserts! (<= stacks-block-height (get end-block policy)) err-policy-expired)
    (asserts! (<= claim-amount (get coverage-amount policy)) err-insufficient-coverage)
    
    (map-set insurance-claims claim-id
      {
        policy-id: policy-id,
        claimant: claimant,
        claim-amount: claim-amount,
        evidence-hash: evidence-hash,
        filed-at: stacks-block-height,
        status: "pending",
        validator: none,
        payout-amount: u0
      }
    )
    
    (var-set total-claims (+ claim-id u1))
    (ok claim-id)
  )
)

(define-public (validate-claim (claim-id uint) (is-valid bool))
  (let (
    (validator tx-sender)
    (claim (unwrap! (map-get? insurance-claims claim-id) err-invalid-claim))
    (policy (unwrap! (map-get? insurance-policies (get policy-id claim)) err-policy-not-found))
  )
    (asserts! (<= (+ (get filed-at claim) claim-window-blocks) stacks-block-height) err-claim-expired)
    (asserts! (is-eq (get status claim) "pending") err-claim-already-processed)
    
    (let (
      (new-status (if is-valid "approved" "rejected"))
      (payout-amount (if is-valid (get claim-amount claim) u0))
    )
      (map-set insurance-claims claim-id
        (merge claim 
          {
            status: new-status,
            validator: (some validator),
            payout-amount: payout-amount
          }
        )
      )
      
      (if is-valid
        (begin
          (var-set insurance-pool (- (var-get insurance-pool) payout-amount))
          (ok payout-amount)
        )
        (ok u0)
      )
    )
  )
)

(define-public (withdraw-underwriter-stake (withdrawal-amount uint))
  (let (
    (underwriter tx-sender)
    (underwriter-details (unwrap! (map-get? underwriter-stakes underwriter) err-insufficient-underwriter-stake))
    (available-stake (- (get stake-amount underwriter-details) (* (get active-policies underwriter-details) minimum-underwriter-stake)))
  )
    (asserts! (<= withdrawal-amount available-stake) err-insufficient-funds)
    (asserts! (is-eq (get active-policies underwriter-details) u0) err-insufficient-funds)
    
    (map-set underwriter-stakes underwriter
      (merge underwriter-details 
        {
          stake-amount: (- (get stake-amount underwriter-details) withdrawal-amount),
          coverage-capacity: (- (get coverage-capacity underwriter-details) (* withdrawal-amount maximum-coverage-ratio))
        }
      )
    )
    
    (var-set total-underwriter-stake (- (var-get total-underwriter-stake) withdrawal-amount))
    (var-set insurance-pool (- (var-get insurance-pool) withdrawal-amount))
    (ok true)
  )
)

(define-read-only (get-insurance-policy (policy-id uint))
  (map-get? insurance-policies policy-id)
)

(define-read-only (get-underwriter-details (underwriter principal))
  (map-get? underwriter-stakes underwriter)
)

(define-read-only (get-insurance-claim (claim-id uint))
  (map-get? insurance-claims claim-id)
)

(define-read-only (get-policy-underwriter (policy-id uint) (underwriter principal))
  (map-get? policy-underwriters {policy-id: policy-id, underwriter: underwriter})
)

(define-read-only (calculate-premium (coverage-amount uint) (duration-blocks uint))
  (let (
    (risk-factor (/ coverage-amount u1000))
    (time-factor (/ duration-blocks u100))
    (base-premium (/ (* coverage-amount base-premium-rate) u10000))
  )
    (+ base-premium (* risk-factor time-factor))
  )
)

(define-read-only (get-insurance-stats)
  (ok {
    total-policies: (var-get total-policies),
    total-claims: (var-get total-claims),
    total-underwriter-stake: (var-get total-underwriter-stake),
    insurance-pool: (var-get insurance-pool)
  })
)

(define-read-only (get-coverage-utilization (underwriter principal))
  (match (map-get? underwriter-stakes underwriter)
    details (ok (/ (* (- (get stake-amount details) (get coverage-capacity details)) u100) (get stake-amount details)))
    (err u0)
  )
)

(define-public (update-underwriter-reputation (underwriter principal) (new-score uint))
  (let (
    (underwriter-details (unwrap! (map-get? underwriter-stakes underwriter) err-insufficient-underwriter-stake))
  )
    (asserts! (and (>= new-score u0) (<= new-score u100)) err-invalid-premium)
    
    (map-set underwriter-stakes underwriter
      (merge underwriter-details {reputation-score: new-score})
    )
    (ok true)
  )
)

(define-public (close-expired-policy (policy-id uint))
  (let (
    (policy (unwrap! (map-get? insurance-policies policy-id) err-policy-not-found))
  )
    (asserts! (> stacks-block-height (get end-block policy)) err-policy-expired)
    (asserts! (get is-active policy) err-policy-expired)
    
    (map-set insurance-policies policy-id
      (merge policy {is-active: false})
    )
    (ok true)
  )
)
