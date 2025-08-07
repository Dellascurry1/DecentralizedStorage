;; Dynamic Market Pricing Engine
;; Automatically adjusts storage prices based on supply/demand dynamics and provider performance

;; Error codes
(define-constant err-unauthorized (err u400))
(define-constant err-invalid-price (err u401))
(define-constant err-provider-not-found (err u402))
(define-constant err-insufficient-data (err u403))
(define-constant err-price-adjustment-too-frequent (err u404))
(define-constant err-market-closed (err u405))
(define-constant err-invalid-bid (err u406))
(define-constant err-price-volatility-exceeded (err u407))

;; Pricing configuration constants
(define-constant base-price-per-gb u100)
(define-constant max-price-adjustment-percent u50)
(define-constant min-adjustment-interval u144) ;; ~1 day in blocks
(define-constant volatility-threshold u25)
(define-constant demand-weight u40)
(define-constant supply-weight u30)
(define-constant performance-weight u30)

;; Market data variables
(define-data-var total-market-supply uint u0)
(define-data-var total-market-demand uint u0)
(define-data-var market-price-index uint u100)
(define-data-var last-price-update uint u0)
(define-data-var market-volatility uint u0)
(define-data-var price-discovery-active bool true)

;; Provider-specific pricing data
(define-map provider-pricing-data
  principal
  {
    base-rate: uint,
    current-rate: uint,
    utilization-ratio: uint,
    performance-multiplier: uint,
    demand-score: uint,
    last-rate-update: uint,
    price-history: (list 10 uint)
  }
)

;; Market supply and demand tracking
(define-map market-metrics
  uint ;; block-height bucket (rounded to nearest 144 blocks)
  {
    supply-volume: uint,
    demand-volume: uint,
    avg-price: uint,
    transaction-count: uint,
    peak-utilization: uint
  }
)

;; Bid/Ask order book for price discovery
(define-map storage-bids
  uint ;; bid-id
  {
    bidder: principal,
    space-needed: uint,
    max-price: uint,
    duration: uint,
    created-at: uint,
    is-active: bool
  }
)

(define-map storage-asks
  uint ;; ask-id
  {
    provider: principal,
    space-available: uint,
    min-price: uint,
    duration: uint,
    created-at: uint,
    is-active: bool
  }
)

(define-data-var next-bid-id uint u0)
(define-data-var next-ask-id uint u0)

;; Performance tracking for pricing adjustments
(define-map provider-performance-history
  principal
  {
    uptime-history: (list 20 uint),
    latency-history: (list 20 uint),
    reliability-score: uint,
    user-satisfaction: uint,
    last-performance-update: uint
  }
)

;; Initialize provider pricing data
(define-public (initialize-provider-pricing (provider principal) (initial-rate uint))
  (begin
    (asserts! (> initial-rate u0) err-invalid-price)
    (asserts! (<= initial-rate (* base-price-per-gb u10)) err-invalid-price)
    
    (map-set provider-pricing-data provider
      {
        base-rate: initial-rate,
        current-rate: initial-rate,
        utilization-ratio: u0,
        performance-multiplier: u100,
        demand-score: u50,
        last-rate-update: stacks-block-height,
        price-history: (list initial-rate)
      }
    )
    (ok true)
  )
)

;; Calculate dynamic price based on multiple factors
(define-public (calculate-dynamic-price (provider principal))
  (let (
    (pricing-data (unwrap! (map-get? provider-pricing-data provider) err-provider-not-found))
    (market-bucket (/ stacks-block-height min-adjustment-interval))
    (market-data (default-to 
      {supply-volume: u0, demand-volume: u0, avg-price: base-price-per-gb, transaction-count: u0, peak-utilization: u0}
      (map-get? market-metrics market-bucket)))
    (supply-demand-ratio (if (> (get demand-volume market-data) u0)
      (/ (* (get supply-volume market-data) u100) (get demand-volume market-data))
      u100))
    (utilization-factor (calculate-utilization-adjustment (get utilization-ratio pricing-data)))
    (performance-factor (get performance-multiplier pricing-data))
    (market-factor (calculate-market-adjustment supply-demand-ratio))
  )
    
    ;; Weighted price calculation
    (let (
      (demand-component (/ (* (get base-rate pricing-data) demand-weight (get demand-score pricing-data)) u10000))
      (supply-component (/ (* (get base-rate pricing-data) supply-weight market-factor) u10000))
      (performance-component (/ (* (get base-rate pricing-data) performance-weight performance-factor) u10000))
      (new-price (+ demand-component supply-component performance-component))
      (price-change-percent (if (> (get current-rate pricing-data) u0)
        (let ((old-price-scaled (* (get current-rate pricing-data) u100))
              (new-price-scaled (* new-price u100)))
          (if (>= new-price-scaled old-price-scaled)
            (- new-price-scaled old-price-scaled)
            (- old-price-scaled new-price-scaled)))
        u0))
    )
      
      ;; Apply volatility controls
      (asserts! (<= price-change-percent (* max-price-adjustment-percent u100)) err-price-volatility-exceeded)
      (asserts! (>= (- stacks-block-height (get last-rate-update pricing-data)) min-adjustment-interval) err-price-adjustment-too-frequent)
      
      ;; Update pricing data
      (map-set provider-pricing-data provider
        (merge pricing-data 
          {
            current-rate: new-price,
            last-rate-update: stacks-block-height,
            price-history: (unwrap-panic (as-max-len? 
              (append (get price-history pricing-data) new-price) 
              u10))
          }
        )
      )
      
      (ok new-price)
    )
  )
)

;; Update market metrics with new transaction data
(define-public (record-market-transaction (space-amount uint) (price-paid uint) (provider principal))
  (let (
    (market-bucket (/ stacks-block-height min-adjustment-interval))
    (current-metrics (default-to 
      {supply-volume: u0, demand-volume: u0, avg-price: base-price-per-gb, transaction-count: u0, peak-utilization: u0}
      (map-get? market-metrics market-bucket)))
    (new-transaction-count (+ (get transaction-count current-metrics) u1))
    (new-demand-volume (+ (get demand-volume current-metrics) space-amount))
    (weighted-avg-price (/ (+ (* (get avg-price current-metrics) (get transaction-count current-metrics)) price-paid) new-transaction-count))
  )
    
    (map-set market-metrics market-bucket
      {
        supply-volume: (get supply-volume current-metrics),
        demand-volume: new-demand-volume,
        avg-price: weighted-avg-price,
        transaction-count: new-transaction-count,
        peak-utilization: (get peak-utilization current-metrics)
      }
    )
    
    ;; Update global demand tracking
    (var-set total-market-demand (+ (var-get total-market-demand) space-amount))
    (update-market-price-index weighted-avg-price)
    (ok true)
  )
)

;; Create a bid for storage space
(define-public (create-storage-bid (space-needed uint) (max-price uint) (duration uint))
  (let (
    (bid-id (var-get next-bid-id))
    (bidder tx-sender)
  )
    (asserts! (> space-needed u0) err-invalid-bid)
    (asserts! (> max-price u0) err-invalid-bid)
    (asserts! (> duration u0) err-invalid-bid)
    
    (map-set storage-bids bid-id
      {
        bidder: bidder,
        space-needed: space-needed,
        max-price: max-price,
        duration: duration,
        created-at: stacks-block-height,
        is-active: true
      }
    )
    
    (var-set next-bid-id (+ bid-id u1))
    (ok bid-id)
  )
)

;; Create an ask for storage space
(define-public (create-storage-ask (space-available uint) (min-price uint) (duration uint))
  (let (
    (ask-id (var-get next-ask-id))
    (provider tx-sender)
  )
    (asserts! (> space-available u0) err-invalid-bid)
    (asserts! (> min-price u0) err-invalid-bid)
    (asserts! (> duration u0) err-invalid-bid)
    
    (map-set storage-asks ask-id
      {
        provider: provider,
        space-available: space-available,
        min-price: min-price,
        duration: duration,
        created-at: stacks-block-height,
        is-active: true
      }
    )
    
    (var-set next-ask-id (+ ask-id u1))
    (update-supply-metrics space-available)
    (ok ask-id)
  )
)

;; Match bids and asks for price discovery
(define-public (execute-bid-ask-match (bid-id uint) (ask-id uint))
  (let (
    (bid (unwrap! (map-get? storage-bids bid-id) err-invalid-bid))
    (ask (unwrap! (map-get? storage-asks ask-id) err-invalid-bid))
    (match-price (/ (+ (get max-price bid) (get min-price ask)) u2))
    (match-space (if (<= (get space-needed bid) (get space-available ask))
                    (get space-needed bid)
                    (get space-available ask)))
  )
    (asserts! (get is-active bid) err-invalid-bid)
    (asserts! (get is-active ask) err-invalid-bid)
    (asserts! (>= (get max-price bid) (get min-price ask)) err-invalid-bid)
    
    ;; Record the matched transaction and update bid/ask status
    (let ((transaction-result (record-market-transaction match-space match-price (get provider ask))))
      (map-set storage-bids bid-id (merge bid {is-active: false}))
      (map-set storage-asks ask-id (merge ask {is-active: false}))
      (ok {matched-price: match-price, matched-space: match-space})
    )
  )
)

;; Update provider utilization for pricing calculations
(define-public (update-provider-utilization (provider principal) (used-space uint) (total-space uint))
  (let (
    (utilization-ratio (if (> total-space u0) (/ (* used-space u100) total-space) u0))
    (pricing-data (unwrap! (map-get? provider-pricing-data provider) err-provider-not-found))
  )
    (map-set provider-pricing-data provider
      (merge pricing-data {utilization-ratio: utilization-ratio})
    )
    (ok utilization-ratio)
  )
)

;; Update provider performance metrics for pricing
(define-public (update-provider-performance (provider principal) (uptime uint) (latency uint) (satisfaction uint))
  (let (
    (performance-data (default-to 
      {uptime-history: (list), latency-history: (list), reliability-score: u100, user-satisfaction: u100, last-performance-update: u0}
      (map-get? provider-performance-history provider)))
    (new-uptime-history (unwrap-panic (as-max-len? (append (get uptime-history performance-data) uptime) u20)))
    (new-latency-history (unwrap-panic (as-max-len? (append (get latency-history performance-data) latency) u20)))
    (reliability-score (calculate-reliability-score new-uptime-history new-latency-history))
    (performance-multiplier (calculate-performance-multiplier reliability-score satisfaction))
  )
    
    (map-set provider-performance-history provider
      {
        uptime-history: new-uptime-history,
        latency-history: new-latency-history,
        reliability-score: reliability-score,
        user-satisfaction: satisfaction,
        last-performance-update: stacks-block-height
      }
    )
    
    ;; Update pricing data with new performance multiplier
    (let ((pricing-data (unwrap! (map-get? provider-pricing-data provider) err-provider-not-found)))
      (map-set provider-pricing-data provider
        (merge pricing-data {performance-multiplier: performance-multiplier})
      )
    )
    
    (ok performance-multiplier)
  )
)

;; Private helper functions

;; Calculate utilization-based price adjustment
(define-private (calculate-utilization-adjustment (utilization-ratio uint))
  (if (> utilization-ratio u80)
    (+ u100 (/ (* (- utilization-ratio u80) u3) u2)) ;; Increase price when utilization > 80%
    (if (< utilization-ratio u20)
      (- u100 (/ (* (- u20 utilization-ratio) u2) u3)) ;; Decrease price when utilization < 20%
      u100 ;; No adjustment for 20-80% utilization
    )
  )
)

;; Calculate market-based price adjustment
(define-private (calculate-market-adjustment (supply-demand-ratio uint))
  (if (< supply-demand-ratio u80) ;; High demand, low supply
    (+ u100 (/ (* (- u80 supply-demand-ratio) u2) u3))
    (if (> supply-demand-ratio u120) ;; Low demand, high supply
      (- u100 (/ (* (- supply-demand-ratio u120) u1) u2))
      u100
    )
  )
)

;; Calculate reliability score from performance history
(define-private (calculate-reliability-score (uptime-history (list 20 uint)) (latency-history (list 20 uint)))
  (let (
    (avg-uptime (if (> (len uptime-history) u0) (/ (fold + uptime-history u0) (len uptime-history)) u100))
    (avg-latency (if (> (len latency-history) u0) (/ (fold + latency-history u0) (len latency-history)) u50))
    (uptime-score (if (<= avg-uptime u100) avg-uptime u100))
    (latency-score (if (< avg-latency u100) (- u100 (/ avg-latency u2)) u50))
  )
    (/ (+ uptime-score latency-score) u2)
  )
)

;; Calculate performance multiplier for pricing
(define-private (calculate-performance-multiplier (reliability-score uint) (satisfaction uint))
  (let (
    (combined-score (/ (+ reliability-score satisfaction) u2))
  )
    (if (> combined-score u90)
      u120 ;; 20% premium for excellent performance
      (if (< combined-score u60)
        u80 ;; 20% discount for poor performance
        (+ u80 (/ (* (- combined-score u60) u40) u30)) ;; Linear scaling
      )
    )
  )
)

;; Update global market price index
(define-private (update-market-price-index (new-price uint))
  (let (
    (current-index (var-get market-price-index))
    (weight u10) ;; Weight for new price
    (new-index (/ (+ (* current-index (- u100 weight)) (* new-price weight)) u100))
  )
    (var-set market-price-index new-index)
    (var-set last-price-update stacks-block-height)
    new-index
  )
)

;; Update supply metrics
(define-private (update-supply-metrics (space-amount uint))
  (var-set total-market-supply (+ (var-get total-market-supply) space-amount))
)

;; Read-only functions

(define-read-only (get-provider-pricing (provider principal))
  (map-get? provider-pricing-data provider)
)

(define-read-only (get-market-metrics (block-bucket uint))
  (map-get? market-metrics block-bucket)
)

(define-read-only (get-current-market-index)
  (ok {
    price-index: (var-get market-price-index),
    total-supply: (var-get total-market-supply),
    total-demand: (var-get total-market-demand),
    last-update: (var-get last-price-update),
    volatility: (var-get market-volatility)
  })
)

(define-read-only (get-storage-bid (bid-id uint))
  (map-get? storage-bids bid-id)
)

(define-read-only (get-storage-ask (ask-id uint))
  (map-get? storage-asks ask-id)
)

(define-read-only (get-provider-performance (provider principal))
  (map-get? provider-performance-history provider)
)

(define-read-only (estimate-price-for-space (provider principal) (space-amount uint))
  (match (map-get? provider-pricing-data provider)
    pricing-data (ok (* (get current-rate pricing-data) space-amount))
    (ok (* base-price-per-gb space-amount))
  )
)


