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

;; ----- Relief Verification and Attestation System -----

;; Verification system error constants
(define-constant ERR-VERIFIER-NOT-FOUND (err u600))
(define-constant ERR-VERIFIER-ALREADY-EXISTS (err u601))
(define-constant ERR-VERIFICATION-NOT-FOUND (err u602))
(define-constant ERR-VERIFICATION-ALREADY-EXISTS (err u603))
(define-constant ERR-ATTESTATION-NOT-FOUND (err u604))
(define-constant ERR-ATTESTATION-ALREADY-EXISTS (err u605))
(define-constant ERR-INSUFFICIENT-VERIFICATIONS (err u606))
(define-constant ERR-VERIFIER-NOT-AUTHORIZED (err u607))
(define-constant ERR-VERIFICATION-EXPIRED (err u608))
(define-constant ERR-INVALID-VERIFICATION-SCORE (err u609))

;; Verification data variables
(define-data-var next-verification-id uint u1)
(define-data-var required-verifications uint u2)
(define-data-var verification-validity-blocks uint u1440) ;; ~10 days

;; Authorized third-party verifiers (NGOs, government agencies, etc.)
(define-map authorized-verifiers
  { verifier: principal }
  {
    organization-name: (string-ascii 60),
    verification-type: (string-ascii 30), ;; "government", "ngo", "community", "medical"
    reputation-score: uint, ;; 0-100
    verifications-completed: uint,
    verifications-disputed: uint,
    authorized-at: uint,
    is-active: bool
  }
)

;; Verification requests and responses
(define-map verification-records
  { verification-id: uint }
  {
    recipient: principal,
    verifier: principal,
    verification-type: (string-ascii 30),
    verification-score: uint, ;; 0-100 confidence level
    verification-notes: (string-ascii 200),
    supporting-documents: (string-ascii 100), ;; IPFS hash or document reference
    created-at: uint,
    expires-at: uint,
    is-valid: bool
  }
)

;; Community attestations for recipients
(define-map community-attestations
  { recipient: principal, attester: principal }
  {
    attestation-type: (string-ascii 20), ;; "neighbor", "employer", "medical", "family"
    confidence-level: uint, ;; 1-5
    attestation-notes: (string-ascii 150),
    attester-reputation: uint,
    created-at: uint
  }
)

;; Verification summary for recipients
(define-map verification-summary
  { recipient: principal }
  {
    total-verifications: uint,
    average-verification-score: uint,
    community-attestations: uint,
    verification-status: (string-ascii 20), ;; "pending", "verified", "disputed"
    last-updated: uint,
    fraud-risk-score: uint ;; 0-100, higher = more risk
  }
)

;; Register a new authorized verifier
(define-public (register-verifier
    (verifier principal)
    (organization-name (string-ascii 60))
    (verification-type (string-ascii 30)))
  (begin
    ;; Only contract owner can register verifiers
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    ;; Check if verifier doesn't already exist
    (asserts! (is-none (map-get? authorized-verifiers { verifier: verifier })) ERR-VERIFIER-ALREADY-EXISTS)
    
    ;; Register verifier
    (map-set authorized-verifiers
      { verifier: verifier }
      {
        organization-name: organization-name,
        verification-type: verification-type,
        reputation-score: u80, ;; Start with good reputation
        verifications-completed: u0,
        verifications-disputed: u0,
        authorized-at: stacks-block-height,
        is-active: true
      }
    )
    
    (ok true)
  )
)

;; Submit verification for a recipient
(define-public (submit-verification
    (recipient principal)
    (verification-type (string-ascii 30))
    (verification-score uint)
    (verification-notes (string-ascii 200))
    (supporting-documents (string-ascii 100)))
  (let (
    (verifier-data (unwrap! (map-get? authorized-verifiers { verifier: tx-sender }) ERR-VERIFIER-NOT-AUTHORIZED))
    (verification-id (var-get next-verification-id))
    (expires-at (+ stacks-block-height (var-get verification-validity-blocks)))
  )
    ;; Check if verifier is active
    (asserts! (get is-active verifier-data) ERR-VERIFIER-NOT-AUTHORIZED)
    ;; Validate verification score
    (asserts! (and (<= verification-score u100) (>= verification-score u0)) ERR-INVALID-VERIFICATION-SCORE)
    ;; Check if recipient is registered
    (asserts! (is-some (map-get? registered-recipients recipient)) ERR-NOT-REGISTERED)
    
    ;; Create verification record
    (map-insert verification-records
      { verification-id: verification-id }
      {
        recipient: recipient,
        verifier: tx-sender,
        verification-type: verification-type,
        verification-score: verification-score,
        verification-notes: verification-notes,
        supporting-documents: supporting-documents,
        created-at: stacks-block-height,
        expires-at: expires-at,
        is-valid: true
      }
    )
    
    ;; Update verifier stats
    (map-set authorized-verifiers
      { verifier: tx-sender }
      (merge verifier-data {
        verifications-completed: (+ (get verifications-completed verifier-data) u1)
      })
    )
    
    ;; Update verification summary for recipient
    (let (
      (current-summary (default-to
        { total-verifications: u0, average-verification-score: u0, community-attestations: u0,
          verification-status: "pending", last-updated: u0, fraud-risk-score: u50 }
        (map-get? verification-summary { recipient: recipient })))
      (new-total (+ (get total-verifications current-summary) u1))
      (new-avg-score (/ (+ (* (get average-verification-score current-summary) (get total-verifications current-summary)) verification-score) new-total))
      (new-status (if (>= new-total (var-get required-verifications)) "verified" "pending"))
    )
      (map-set verification-summary
        { recipient: recipient }
        {
          total-verifications: new-total,
          average-verification-score: new-avg-score,
          community-attestations: (get community-attestations current-summary),
          verification-status: new-status,
          last-updated: stacks-block-height,
          fraud-risk-score: (if (> new-avg-score u80) u10 u30) ;; Lower risk for high scores
        }
      )
    )
    
    (var-set next-verification-id (+ verification-id u1))
    (ok verification-id)
  )
)

;; Submit community attestation for a recipient
(define-public (submit-attestation
    (recipient principal)
    (attestation-type (string-ascii 20))
    (confidence-level uint)
    (attestation-notes (string-ascii 150)))
  (let (
    (attester tx-sender)
    (existing-attestation (map-get? community-attestations { recipient: recipient, attester: attester }))
    ;; Calculate attester reputation based on their own verification status
    (attester-verification (default-to
      { total-verifications: u0, average-verification-score: u0, community-attestations: u0,
        verification-status: "pending", last-updated: u0, fraud-risk-score: u50 }
      (map-get? verification-summary { recipient: attester })))
    (attester-rep (if (> (get average-verification-score attester-verification) u70) u80 u40))
  )
    ;; Check if recipient is registered
    (asserts! (is-some (map-get? registered-recipients recipient)) ERR-NOT-REGISTERED)
    ;; Validate confidence level
    (asserts! (and (<= confidence-level u5) (>= confidence-level u1)) ERR-INVALID-VERIFICATION-SCORE)
    ;; Prevent self-attestation
    (asserts! (not (is-eq attester recipient)) ERR-NOT-AUTHORIZED)
    ;; Check if attestation already exists
    (asserts! (is-none existing-attestation) ERR-ATTESTATION-ALREADY-EXISTS)
    
    ;; Create attestation
    (map-insert community-attestations
      { recipient: recipient, attester: attester }
      {
        attestation-type: attestation-type,
        confidence-level: confidence-level,
        attestation-notes: attestation-notes,
        attester-reputation: attester-rep,
        created-at: stacks-block-height
      }
    )
    
    ;; Update recipient verification summary
    (let (
      (current-summary (default-to
        { total-verifications: u0, average-verification-score: u0, community-attestations: u0,
          verification-status: "pending", last-updated: u0, fraud-risk-score: u50 }
        (map-get? verification-summary { recipient: recipient })))
      (new-attestations (+ (get community-attestations current-summary) u1))
      (fraud-adjustment (if (> confidence-level u3) u5 u0)) ;; High confidence reduces fraud risk
      (new-fraud-risk (if (> (get fraud-risk-score current-summary) fraud-adjustment)
        (- (get fraud-risk-score current-summary) fraud-adjustment)
        u0))
    )
      (map-set verification-summary
        { recipient: recipient }
        (merge current-summary {
          community-attestations: new-attestations,
          last-updated: stacks-block-height,
          fraud-risk-score: new-fraud-risk
        })
      )
    )
    
    (ok true)
  )
)

;; Claim relief with verification requirements
(define-public (claim-verified-relief)
  (let (
    (recipient tx-sender)
    (recipient-data (unwrap! (map-get? registered-recipients recipient) ERR-NOT-REGISTERED))
    (verification-data (unwrap! (map-get? verification-summary { recipient: recipient }) ERR-NOT-REGISTERED))
    (amount (var-get relief-amount-per-person))
  )
    ;; Check basic eligibility
    (asserts! (var-get distribution-active) ERR-NOT-AUTHORIZED)
    (asserts! (not (get claimed recipient-data)) ERR-ALREADY-CLAIMED)
    (asserts! (>= (var-get total-funds) amount) ERR-INSUFFICIENT-FUNDS)
    
    ;; Check verification requirements
    (asserts! (>= (get total-verifications verification-data) (var-get required-verifications)) ERR-INSUFFICIENT-FUNDS)
    (asserts! (is-eq (get verification-status verification-data) "verified") ERR-NOT-AUTHORIZED)
    (asserts! (<= (get fraud-risk-score verification-data) u30) ERR-NOT-AUTHORIZED) ;; Low fraud risk required
    
    ;; Execute transfer
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (var-set total-funds (- (var-get total-funds) amount))
    
    ;; Update recipient status
    (map-set registered-recipients recipient (merge recipient-data { claimed: true }))
    (map-set distribution-records recipient {
      amount: amount,
      distribution-time: stacks-block-height,
      transaction-id: (concat "VRF-" (int-to-ascii stacks-block-height))
    })
    
    (ok amount)
  )
)

;; Update verifier reputation based on dispute outcomes
(define-public (update-verifier-reputation
    (verifier principal)
    (dispute-outcome bool)) ;; true = verifier was correct, false = verifier was wrong
  (let (
    (verifier-data (unwrap! (map-get? authorized-verifiers { verifier: verifier }) ERR-VERIFIER-NOT-FOUND))
    (current-rep (get reputation-score verifier-data))
    (adjustment (if dispute-outcome u5 u10)) ;; Gain 5 for correct, lose 10 for incorrect
    (new-reputation (if dispute-outcome
      (if (> (+ current-rep adjustment) u100) u100 (+ current-rep adjustment))
      (if (> current-rep adjustment) (- current-rep adjustment) u0)))
  )
    ;; Only contract owner can update reputation
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    
    ;; Update verifier reputation
    (map-set authorized-verifiers
      { verifier: verifier }
      (merge verifier-data {
        reputation-score: new-reputation,
        verifications-disputed: (if dispute-outcome
          (get verifications-disputed verifier-data)
          (+ (get verifications-disputed verifier-data) u1))
      })
    )
    
    ;; Deactivate verifier if reputation too low
    (if (< new-reputation u30)
      (map-set authorized-verifiers
        { verifier: verifier }
        (merge verifier-data { is-active: false })
      )
      true
    )
    
    (ok new-reputation)
  )
)

;; Set verification requirements
(define-public (set-verification-requirements (required-count uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= required-count u1) (<= required-count u5)) ERR-INVALID-AMOUNT)
    (var-set required-verifications required-count)
    (ok true)
  )
)

;; Read-only functions for verification system
(define-read-only (get-verifier-info (verifier principal))
  (map-get? authorized-verifiers { verifier: verifier })
)

(define-read-only (get-verification-record (verification-id uint))
  (map-get? verification-records { verification-id: verification-id })
)

(define-read-only (get-verification-summary (recipient principal))
  (map-get? verification-summary { recipient: recipient })
)

(define-read-only (get-community-attestation (recipient principal) (attester principal))
  (map-get? community-attestations { recipient: recipient, attester: attester })
)

(define-read-only (is-recipient-verified (recipient principal))
  (match (map-get? verification-summary { recipient: recipient })
    summary (and
      (is-eq (get verification-status summary) "verified")
      (<= (get fraud-risk-score summary) u30)
      (>= (get total-verifications summary) (var-get required-verifications))
    )
    false
  )
)

(define-read-only (get-verification-requirements)
  (ok {
    required-verifications: (var-get required-verifications),
    verification-validity-blocks: (var-get verification-validity-blocks),
    max-fraud-risk-score: u30
  })
)

(define-read-only (calculate-recipient-trust-score (recipient principal))
  (match (map-get? verification-summary { recipient: recipient })
    summary (let (
      (verification-weight (* (get average-verification-score summary) u60))
      (attestation-weight (* (if (> (get community-attestations summary) u10) u10 (get community-attestations summary)) u40))
      (fraud-penalty (* (get fraud-risk-score summary) u100))
      (trust-score (if (> (+ verification-weight attestation-weight) fraud-penalty)
        (/ (- (+ verification-weight attestation-weight) fraud-penalty) u100)
        u0))
    )
      (ok (if (> trust-score u100) u100 trust-score))
    )
    (ok u0)
  )
)


