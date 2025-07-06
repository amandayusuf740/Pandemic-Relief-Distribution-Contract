(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-REGISTERED (err u101))
(define-constant ERR-NOT-REGISTERED (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-INVALID-AMOUNT (err u104))
(define-constant ERR-ALREADY-CLAIMED (err u105))

(define-data-var contract-owner principal tx-sender)
(define-data-var total-funds uint u0)
(define-data-var distribution-active bool false)
(define-data-var relief-amount-per-person uint u0)

(define-map registered-recipients 
    principal 
    {
        name: (string-ascii 50),
        status: (string-ascii 20),
        registration-time: uint,
        claimed: bool
    }
)

(define-map distribution-records
    principal
    {
        amount: uint,
        distribution-time: uint,
        transaction-id: (string-ascii 100)
    }
)

(define-public (set-contract-owner (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (var-set contract-owner new-owner))
    )
)

(define-public (register-recipient (recipient-name (string-ascii 50)) (status (string-ascii 20)))
    (let ((recipient tx-sender))
        (asserts! (is-none (map-get? registered-recipients recipient)) ERR-ALREADY-REGISTERED)
        (ok (map-set registered-recipients 
            recipient
            {
                name: recipient-name,
                status: status,
                registration-time: stacks-block-height,
                claimed: false
            }
        ))
    )
)

(define-public (fund-contract (amount uint))
    (begin
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set total-funds (+ (var-get total-funds) amount))
        (ok true)
    )
)

(define-public (set-distribution-amount (amount uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (ok (var-set relief-amount-per-person amount))
    )
)

(define-public (activate-distribution)
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (var-set distribution-active true))
    )
)

(define-public (deactivate-distribution)
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (var-set distribution-active false))
    )
)

(define-public (claim-relief)
    (let (
        (recipient tx-sender)
        (recipient-data (unwrap! (map-get? registered-recipients recipient) ERR-NOT-REGISTERED))
        (amount (var-get relief-amount-per-person))
        )
        (asserts! (var-get distribution-active) ERR-NOT-AUTHORIZED)
        (asserts! (not (get claimed recipient-data)) ERR-ALREADY-CLAIMED)
        (asserts! (>= (var-get total-funds) amount) ERR-INSUFFICIENT-FUNDS)
        
        (try! (as-contract (stx-transfer? amount tx-sender recipient)))
        (var-set total-funds (- (var-get total-funds) amount))
        
        (map-set registered-recipients recipient (merge recipient-data {claimed: true}))
        (map-set distribution-records recipient {
            amount: amount,
            distribution-time: stacks-block-height,
            transaction-id: (concat (int-to-ascii stacks-block-height) (concat "-" (int-to-ascii amount)))
        })
        (ok true)
    )
)

(define-read-only (get-recipient-info (recipient principal))
    (map-get? registered-recipients recipient)
)

(define-read-only (get-distribution-info (recipient principal))
    (map-get? distribution-records recipient)
)

(define-read-only (get-contract-info)
    (ok {
        owner: (var-get contract-owner),
        total-funds: (var-get total-funds),
        distribution-active: (var-get distribution-active),
        relief-amount: (var-get relief-amount-per-person)
    })
)


(define-public (batch-register-recipients 
    (recipients (list 200 principal))
    (names (list 200 (string-ascii 50)))
    (statuses (list 200 (string-ascii 20))))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (len recipients) (len names)) ERR-INVALID-AMOUNT)
        (asserts! (is-eq (len names) (len statuses)) ERR-INVALID-AMOUNT)
        (ok (map register-recipient-internal recipients names statuses))
    )
)

(define-private (register-recipient-internal 
    (recipient principal) 
    (name (string-ascii 50))
    (status (string-ascii 20)))
    (if (is-none (map-get? registered-recipients recipient))
        (map-set registered-recipients 
            recipient
            {
                name: name,
                status: status,
                registration-time: stacks-block-height,
                claimed: false
            }
        )
        false
    )
)


(define-constant ERR-NO-FUNDS (err u106))

(define-public (emergency-withdraw (amount uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (<= amount (var-get total-funds)) ERR-INSUFFICIENT-FUNDS)
        (asserts! (not (var-get distribution-active)) ERR-NOT-AUTHORIZED)
        
        (try! (as-contract (stx-transfer? amount tx-sender (var-get contract-owner))))
        (var-set total-funds (- (var-get total-funds) amount))
        (ok true)
    )
)


(define-constant ERR-ROUND-NOT-FOUND (err u107))
(define-constant ERR-ROUND-NOT-ACTIVE (err u108))
(define-constant ERR-ROUND-ALREADY-EXISTS (err u109))

(define-data-var current-round-id uint u0)
(define-data-var next-round-id uint u1)

(define-map distribution-rounds
    uint
    {
        round-name: (string-ascii 50),
        relief-amount: uint,
        start-block: uint,
        end-block: uint,
        is-active: bool,
        total-distributed: uint,
        recipients-count: uint
    }
)

(define-map round-claims
    {round-id: uint, recipient: principal}
    {
        amount: uint,
        claim-time: uint,
        claim-block: uint
    }
)

(define-public (create-distribution-round 
    (round-name (string-ascii 50))
    (relief-amount uint)
    (duration-blocks uint))
    (let (
        (round-id (var-get next-round-id))
        (start-block stacks-block-height)
        (end-block (+ stacks-block-height duration-blocks))
        )
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (> relief-amount u0) ERR-INVALID-AMOUNT)
        (asserts! (> duration-blocks u0) ERR-INVALID-AMOUNT)
        (asserts! (is-none (map-get? distribution-rounds round-id)) ERR-ROUND-ALREADY-EXISTS)
        
        (map-set distribution-rounds round-id {
            round-name: round-name,
            relief-amount: relief-amount,
            start-block: start-block,
            end-block: end-block,
            is-active: true,
            total-distributed: u0,
            recipients-count: u0
        })
        (var-set current-round-id round-id)
        (var-set next-round-id (+ round-id u1))
        (ok round-id)
    )
)

(define-public (claim-round-relief (round-id uint))
    (let (
        (recipient tx-sender)
        (recipient-data (unwrap! (map-get? registered-recipients recipient) ERR-NOT-REGISTERED))
        (round-data (unwrap! (map-get? distribution-rounds round-id) ERR-ROUND-NOT-FOUND))
        (relief-amount (get relief-amount round-data))
        (claim-key {round-id: round-id, recipient: recipient})
        )
        (asserts! (get is-active round-data) ERR-ROUND-NOT-ACTIVE)
        (asserts! (>= stacks-block-height (get start-block round-data)) ERR-ROUND-NOT-ACTIVE)
        (asserts! (<= stacks-block-height (get end-block round-data)) ERR-ROUND-NOT-ACTIVE)
        (asserts! (is-none (map-get? round-claims claim-key)) ERR-ALREADY-CLAIMED)
        (asserts! (>= (var-get total-funds) relief-amount) ERR-INSUFFICIENT-FUNDS)
        
        (try! (as-contract (stx-transfer? relief-amount tx-sender recipient)))
        (var-set total-funds (- (var-get total-funds) relief-amount))
        
        (map-set round-claims claim-key {
            amount: relief-amount,
            claim-time: stacks-block-height,
            claim-block: stacks-block-height
        })
        
        (map-set distribution-rounds round-id 
            (merge round-data {
                total-distributed: (+ (get total-distributed round-data) relief-amount),
                recipients-count: (+ (get recipients-count round-data) u1)
            })
        )
        (ok true)
    )
)

(define-public (deactivate-round (round-id uint))
    (let (
        (round-data (unwrap! (map-get? distribution-rounds round-id) ERR-ROUND-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (map-set distribution-rounds round-id (merge round-data {is-active: false}))
        (ok true)
    )
)

(define-public (extend-round (round-id uint) (additional-blocks uint))
    (let (
        (round-data (unwrap! (map-get? distribution-rounds round-id) ERR-ROUND-NOT-FOUND))
        (new-end-block (+ (get end-block round-data) additional-blocks))
        )
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (> additional-blocks u0) ERR-INVALID-AMOUNT)
        (map-set distribution-rounds round-id (merge round-data {end-block: new-end-block}))
        (ok new-end-block)
    )
)

(define-read-only (get-round-info (round-id uint))
    (map-get? distribution-rounds round-id)
)

(define-read-only (get-recipient-round-claim (round-id uint) (recipient principal))
    (map-get? round-claims {round-id: round-id, recipient: recipient})
)

(define-read-only (has-claimed-round (round-id uint) (recipient principal))
    (is-some (map-get? round-claims {round-id: round-id, recipient: recipient}))
)

(define-read-only (get-current-round-id)
    (ok (var-get current-round-id))
)

(define-read-only (get-active-rounds)
    (ok {
        current-round: (var-get current-round-id),
        next-round: (var-get next-round-id)
    })
)

(define-read-only (is-round-active (round-id uint))
    (match (map-get? distribution-rounds round-id)
        round-data (and 
            (get is-active round-data)
            (>= stacks-block-height (get start-block round-data))
            (<= stacks-block-height (get end-block round-data))
        )
        false
    )
)