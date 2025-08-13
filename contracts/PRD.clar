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

(define-constant ERR-NOT-GUARDIAN (err u110))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u111))
(define-constant ERR-PROPOSAL-EXPIRED (err u112))
(define-constant ERR-ALREADY-VOTED (err u113))
(define-constant ERR-PROPOSAL-NOT-PASSED (err u114))
(define-constant ERR-PROPOSAL-ALREADY-EXECUTED (err u115))
(define-constant ERR-INSUFFICIENT-SIGNATURES (err u116))

(define-data-var proposal-counter uint u0)
(define-data-var required-signatures uint u2)
(define-data-var guardian-count uint u0)

(define-map guardians principal bool)

(define-map proposals
    uint
    {
        proposer: principal,
        proposal-type: (string-ascii 20),
        target-amount: uint,
        target-recipient: (optional principal),
        description: (string-ascii 200),
        created-at: uint,
        expires-at: uint,
        yes-votes: uint,
        no-votes: uint,
        executed: bool,
        passed: bool
    }
)

(define-map proposal-votes
    {proposal-id: uint, voter: principal}
    {vote: bool, voted-at: uint}
)
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


(define-public (add-guardian (guardian principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (not (default-to false (map-get? guardians guardian))) ERR-ALREADY-REGISTERED)
        (map-set guardians guardian true)
        (var-set guardian-count (+ (var-get guardian-count) u1))
        (ok true)
    )
)

(define-public (remove-guardian (guardian principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (default-to false (map-get? guardians guardian)) ERR-NOT-REGISTERED)
        (map-set guardians guardian false)
        (var-set guardian-count (- (var-get guardian-count) u1))
        (ok true)
    )
)

(define-public (set-required-signatures (count uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (> count u0) ERR-INVALID-AMOUNT)
        (asserts! (<= count (var-get guardian-count)) ERR-INVALID-AMOUNT)
        (ok (var-set required-signatures count))
    )
)

(define-public (create-proposal 
    (proposal-type (string-ascii 20))
    (target-amount uint)
    (target-recipient (optional principal))
    (description (string-ascii 200))
    (duration-blocks uint))
    (let (
        (proposal-id (+ (var-get proposal-counter) u1))
        (proposer tx-sender)
        (created-at stacks-block-height)
        (expires-at (+ stacks-block-height duration-blocks))
        )
        (asserts! (default-to false (map-get? guardians proposer)) ERR-NOT-GUARDIAN)
        (asserts! (> duration-blocks u0) ERR-INVALID-AMOUNT)
        
        (map-set proposals proposal-id {
            proposer: proposer,
            proposal-type: proposal-type,
            target-amount: target-amount,
            target-recipient: target-recipient,
            description: description,
            created-at: created-at,
            expires-at: expires-at,
            yes-votes: u0,
            no-votes: u0,
            executed: false,
            passed: false
        })
        
        (var-set proposal-counter proposal-id)
        (ok proposal-id)
    )
)

(define-public (vote-proposal (proposal-id uint) (vote bool))
    (let (
        (voter tx-sender)
        (proposal-data (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (vote-key {proposal-id: proposal-id, voter: voter})
        )
        (asserts! (default-to false (map-get? guardians voter)) ERR-NOT-GUARDIAN)
        (asserts! (<= stacks-block-height (get expires-at proposal-data)) ERR-PROPOSAL-EXPIRED)
        (asserts! (is-none (map-get? proposal-votes vote-key)) ERR-ALREADY-VOTED)
        
        (map-set proposal-votes vote-key {
            vote: vote,
            voted-at: stacks-block-height
        })
        
        (let (
            (new-yes-votes (if vote (+ (get yes-votes proposal-data) u1) (get yes-votes proposal-data)))
            (new-no-votes (if vote (get no-votes proposal-data) (+ (get no-votes proposal-data) u1)))
            (is-passed (>= new-yes-votes (var-get required-signatures)))
            )
            (map-set proposals proposal-id (merge proposal-data {
                yes-votes: new-yes-votes,
                no-votes: new-no-votes,
                passed: is-passed
            }))
        )
        (ok true)
    )
)


(define-read-only (get-proposal-info (proposal-id uint))
    (map-get? proposals proposal-id)
)

(define-read-only (get-proposal-vote (proposal-id uint) (voter principal))
    (map-get? proposal-votes {proposal-id: proposal-id, voter: voter})
)

(define-read-only (is-guardian (address principal))
    (default-to false (map-get? guardians address))
)

(define-read-only (get-governance-info)
    (ok {
        guardian-count: (var-get guardian-count),
        required-signatures: (var-get required-signatures),
        proposal-count: (var-get proposal-counter)
    })
)

;; Geographic Distribution Zones Feature
;; Enables zone-based relief distribution with varying amounts per region

(define-constant ERR-ZONE-NOT-FOUND (err u117))
(define-constant ERR-ZONE-ALREADY-EXISTS (err u118))
(define-constant ERR-ZONE-INACTIVE (err u119))
(define-constant ERR-RECIPIENT-ZONE-MISMATCH (err u120))
(define-constant ERR-NOT-ZONE-ADMIN (err u121))

(define-data-var zone-counter uint u0)

;; Map to store zone information
(define-map distribution-zones
    uint ;; zone-id
    {
        zone-name: (string-ascii 50),
        zone-code: (string-ascii 10), ;; e.g., "NY", "CA", "TX"
        relief-multiplier: uint, ;; percentage multiplier (100 = 1x, 150 = 1.5x, 200 = 2x)
        max-recipients: uint,
        current-recipients: uint,
        is-active: bool,
        created-at: uint,
        zone-admin: principal,
        special-conditions: (string-ascii 100) ;; "high-risk", "rural", "urban", etc.
    }
)

;; Map to store recipient-zone assignments
(define-map recipient-zones
    principal ;; recipient
    {
        zone-id: uint,
        assigned-at: uint,
        verified: bool,
        zone-admin-approval: bool
    }
)

;; Map to track zone-specific claims
(define-map zone-claims
    {zone-id: uint, recipient: principal}
    {
        base-amount: uint,
        zone-adjusted-amount: uint,
        claim-timestamp: uint,
        verification-status: (string-ascii 20)
    }
)

;; Zone administrators management
(define-map zone-admins principal bool)

;; Create a new distribution zone
(define-public (create-distribution-zone 
    (zone-name (string-ascii 50))
    (zone-code (string-ascii 10))
    (relief-multiplier uint)
    (max-recipients uint)
    (zone-admin principal)
    (special-conditions (string-ascii 100)))
    (let (
        (zone-id (+ (var-get zone-counter) u1))
        )
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (> relief-multiplier u0) ERR-INVALID-AMOUNT)
        (asserts! (> max-recipients u0) ERR-INVALID-AMOUNT)
        (asserts! (<= relief-multiplier u300) ERR-INVALID-AMOUNT) ;; max 3x multiplier
        
        (map-set distribution-zones zone-id {
            zone-name: zone-name,
            zone-code: zone-code,
            relief-multiplier: relief-multiplier,
            max-recipients: max-recipients,
            current-recipients: u0,
            is-active: true,
            created-at: stacks-block-height,
            zone-admin: zone-admin,
            special-conditions: special-conditions
        })
        
        (map-set zone-admins zone-admin true)
        (var-set zone-counter zone-id)
        (ok zone-id)
    )
)

;; Assign recipient to a zone
(define-public (assign-recipient-to-zone (recipient principal) (zone-id uint))
    (let (
        (zone-data (unwrap! (map-get? distribution-zones zone-id) ERR-ZONE-NOT-FOUND))
        (recipient-data (unwrap! (map-get? registered-recipients recipient) ERR-NOT-REGISTERED))
        )
        (asserts! (or 
            (is-eq tx-sender (var-get contract-owner))
            (is-eq tx-sender (get zone-admin zone-data))
        ) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active zone-data) ERR-ZONE-INACTIVE)
        (asserts! (< (get current-recipients zone-data) (get max-recipients zone-data)) ERR-INSUFFICIENT-FUNDS)
        
        ;; Check if recipient is already assigned to this zone
        (asserts! (is-none (map-get? recipient-zones recipient)) ERR-ALREADY-REGISTERED)
        
        (map-set recipient-zones recipient {
            zone-id: zone-id,
            assigned-at: stacks-block-height,
            verified: false,
            zone-admin-approval: (is-eq tx-sender (get zone-admin zone-data))
        })
        
        ;; Update zone recipient count
        (map-set distribution-zones zone-id 
            (merge zone-data {current-recipients: (+ (get current-recipients zone-data) u1)})
        )
        
        (ok true)
    )
)

;; Verify recipient's zone assignment
(define-public (verify-zone-assignment (recipient principal))
    (let (
        (zone-assignment (unwrap! (map-get? recipient-zones recipient) ERR-NOT-REGISTERED))
        (zone-data (unwrap! (map-get? distribution-zones (get zone-id zone-assignment)) ERR-ZONE-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get zone-admin zone-data)) ERR-NOT-ZONE-ADMIN)
        
        (map-set recipient-zones recipient 
            (merge zone-assignment {
                verified: true,
                zone-admin-approval: true
            })
        )
        (ok true)
    )
)

;; Claim zone-adjusted relief
(define-public (claim-zone-relief)
    (let (
        (recipient tx-sender)
        (recipient-data (unwrap! (map-get? registered-recipients recipient) ERR-NOT-REGISTERED))
        (zone-assignment (unwrap! (map-get? recipient-zones recipient) ERR-RECIPIENT-ZONE-MISMATCH))
        (zone-data (unwrap! (map-get? distribution-zones (get zone-id zone-assignment)) ERR-ZONE-NOT-FOUND))
        (base-amount (var-get relief-amount-per-person))
        (multiplier (get relief-multiplier zone-data))
        (adjusted-amount (/ (* base-amount multiplier) u100))
        (claim-key {zone-id: (get zone-id zone-assignment), recipient: recipient})
        )
        (asserts! (var-get distribution-active) ERR-NOT-AUTHORIZED)
        (asserts! (not (get claimed recipient-data)) ERR-ALREADY-CLAIMED)
        (asserts! (get is-active zone-data) ERR-ZONE-INACTIVE)
        (asserts! (get verified zone-assignment) ERR-NOT-AUTHORIZED)
        (asserts! (>= (var-get total-funds) adjusted-amount) ERR-INSUFFICIENT-FUNDS)
        (asserts! (is-none (map-get? zone-claims claim-key)) ERR-ALREADY-CLAIMED)
        
        ;; Execute transfer
        (try! (as-contract (stx-transfer? adjusted-amount tx-sender recipient)))
        (var-set total-funds (- (var-get total-funds) adjusted-amount))
        
        ;; Update recipient status
        (map-set registered-recipients recipient (merge recipient-data {claimed: true}))
        
        ;; Record zone claim
        (map-set zone-claims claim-key {
            base-amount: base-amount,
            zone-adjusted-amount: adjusted-amount,
            claim-timestamp: stacks-block-height,
            verification-status: "completed"
        })
        
        ;; Update distribution records
        (map-set distribution-records recipient {
            amount: adjusted-amount,
            distribution-time: stacks-block-height,
            transaction-id: (concat (concat (get zone-code zone-data) "-") 
                           (int-to-ascii stacks-block-height))
        })
        
        (ok adjusted-amount)
    )
)

;; Update zone relief multiplier
(define-public (update-zone-multiplier (zone-id uint) (new-multiplier uint))
    (let (
        (zone-data (unwrap! (map-get? distribution-zones zone-id) ERR-ZONE-NOT-FOUND))
        )
        (asserts! (or 
            (is-eq tx-sender (var-get contract-owner))
            (is-eq tx-sender (get zone-admin zone-data))
        ) ERR-NOT-AUTHORIZED)
        (asserts! (> new-multiplier u0) ERR-INVALID-AMOUNT)
        (asserts! (<= new-multiplier u300) ERR-INVALID-AMOUNT)
        
        (map-set distribution-zones zone-id 
            (merge zone-data {relief-multiplier: new-multiplier})
        )
        (ok true)
    )
)

;; Deactivate a zone
(define-public (deactivate-zone (zone-id uint))
    (let (
        (zone-data (unwrap! (map-get? distribution-zones zone-id) ERR-ZONE-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        
        (map-set distribution-zones zone-id 
            (merge zone-data {is-active: false})
        )
        (ok true)
    )
)

;; Read-only functions for zone management

(define-read-only (get-zone-info (zone-id uint))
    (map-get? distribution-zones zone-id)
)

(define-read-only (get-recipient-zone (recipient principal))
    (map-get? recipient-zones recipient)
)

(define-read-only (get-zone-claim-info (zone-id uint) (recipient principal))
    (map-get? zone-claims {zone-id: zone-id, recipient: recipient})
)

(define-read-only (calculate-zone-relief (zone-id uint))
    (match (map-get? distribution-zones zone-id)
        zone-data 
            (let (
                (base-amount (var-get relief-amount-per-person))
                (multiplier (get relief-multiplier zone-data))
                )
                (ok (/ (* base-amount multiplier) u100))
            )
        ERR-ZONE-NOT-FOUND
    )
)

(define-read-only (get-zone-statistics (zone-id uint))
    (match (map-get? distribution-zones zone-id)
        zone-data 
            (ok {
                zone-name: (get zone-name zone-data),
                zone-code: (get zone-code zone-data),
                current-recipients: (get current-recipients zone-data),
                max-recipients: (get max-recipients zone-data),
                capacity-percentage: (/ (* (get current-recipients zone-data) u100) (get max-recipients zone-data)),
                relief-multiplier: (get relief-multiplier zone-data),
                is-active: (get is-active zone-data)
            })
        ERR-ZONE-NOT-FOUND
    )
)

(define-read-only (is-zone-admin (address principal))
    (default-to false (map-get? zone-admins address))
)

(define-read-only (get-total-zones)
    (ok (var-get zone-counter))
)








