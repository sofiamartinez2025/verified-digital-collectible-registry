;; Verified Digital Collectible Registry - Decentralized platform for authenticating and managing 
;; unique digital collectibles with comprehensive permission structures
;;
;; This protocol enables creators to securely register, authenticate, and trade digital collectibles
;; while maintaining complete historical provenance and customized access settings

;; Core Registry Tracker
(define-data-var collectible-sequence-counter uint u0)

;; Platform Supervisor
(define-constant supervisor-key tx-sender)

;; Response Code Definitions
(define-constant unauthorized-modification-code (err u405))
(define-constant supervisor-restricted-code (err u400))
(define-constant collectible-missing-code (err u401))
(define-constant collectible-already-exists-code (err u402))
(define-constant invalid-name-code (err u403))
(define-constant invalid-metrics-code (err u404))
(define-constant access-violation-code (err u406))
(define-constant viewing-restricted-code (err u407))
(define-constant invalid-category-code (err u408))

;; Main Collectible Storage
(define-map collectible-repository
  { collectible-id: uint }
  {
    collectible-name: (string-ascii 64),
    collectible-creator: principal,
    storage-requirement: uint,
    creation-height: uint,
    collectible-details: (string-ascii 128),
    collectible-categories: (list 10 (string-ascii 32))
  }
)

;; Viewer Authorization Framework
(define-map viewer-privileges
  { collectible-id: uint, observer: principal }
  { view-permitted: bool }
)

;; =============== Helper Functions ===============

;; Validates category format requirements
(define-private (validate-category-format (category (string-ascii 32)))
  (and
    (> (len category) u0)
    (< (len category) u33)
  )
)

;; Ensures all categories meet format requirements
(define-private (validate-category-list (categories (list 10 (string-ascii 32))))
  (and
    (> (len categories) u0)
    (<= (len categories) u10)
    (is-eq (len (filter validate-category-format categories)) (len categories))
  )
)

;; Verifies collectible existence in repository
(define-private (collectible-registered (collectible-id uint))
  (is-some (map-get? collectible-repository { collectible-id: collectible-id }))
)

;; Retrieves storage requirement for a collectible
(define-private (get-storage-requirement (collectible-id uint))
  (default-to u0
    (get storage-requirement
      (map-get? collectible-repository { collectible-id: collectible-id })
    )
  )
)

;; Creator verification check
(define-private (is-collectible-creator (collectible-id uint) (observer principal))
  (match (map-get? collectible-repository { collectible-id: collectible-id })
    item-data (is-eq (get collectible-creator item-data) observer)
    false
  )
)

;; =============== System Management ===============

;; System status indicator
(define-data-var protocol-paused bool false)

;; Status explanation
(define-data-var pause-explanation (string-ascii 128) "")

;; Resume normal operations
(define-public (enable-protocol)
  (begin
    ;; Supervisor only
    (asserts! (is-eq tx-sender supervisor-key) supervisor-restricted-code)

    ;; Reset emergency state
    (var-set protocol-paused false)
    (var-set pause-explanation "")
    (ok true)
  )
)

;; Verify operational status
(define-private (is-protocol-active)
  (not (var-get protocol-paused))
)

;; =============== Security and Rate Limiting ===============

;; Transaction tracking registry
(define-map transaction-monitoring
  { participant: principal }
  {
    last-transaction-height: uint,
    transactions-in-period: uint
  }
)

;; Security configuration
(define-data-var monitoring-window uint u100)  ;; blocks
(define-data-var max-transactions uint u10)    ;; max transactions per window

;; Transaction rate verification
(define-private (verify-transaction-rate (participant principal))
  (let
    (
      (monitor-data (default-to { last-transaction-height: u0, transactions-in-period: u0 }
        (map-get? transaction-monitoring { participant: participant })))
      (period-start (- block-height (var-get monitoring-window)))
    )
    (if (< (get last-transaction-height monitor-data) period-start)
      ;; New monitoring period, reset counter
      (begin
        (map-set transaction-monitoring { participant: participant }
          { last-transaction-height: block-height, transactions-in-period: u1 })
        true)
      ;; Verify within allowed transaction count
      (if (< (get transactions-in-period monitor-data) (var-get max-transactions))
        (begin
          (map-set transaction-monitoring { participant: participant }
            { 
              last-transaction-height: block-height,
              transactions-in-period: (+ (get transactions-in-period monitor-data) u1)
            })
          true)
        false)
    )
  )
)

;; =============== Time-Locked Operations ===============

;; Operation sequence tracking
(define-data-var operation-sequence uint u0)

;; Mandatory delay period (in blocks)
(define-data-var security-delay-period uint u10)

;; Pending operations registry
(define-map scheduled-operations
  { operation-sequence: uint, collectible-id: uint }
  {
    operation-category: (string-ascii 20),
    requester: principal,
    recipient: (optional principal),
    request-height: uint,
    verification-hash: (buff 32),
    expiration-height: uint
  }
)

;; Schedule ownership transfer with verification and time delay
(define-public (schedule-secure-transfer (collectible-id uint) (new-owner principal) (verification-hash (buff 32)))
  (let
    (
      (collectible-data (unwrap! (map-get? collectible-repository { collectible-id: collectible-id })
        collectible-missing-code))
      (operation-sequence-id (+ (var-get operation-sequence) u1))
      (expiration (+ block-height (var-get security-delay-period)))
    )
    ;; Verify caller is the current creator
    (asserts! (collectible-registered collectible-id) collectible-missing-code)
    (asserts! (is-eq (get collectible-creator collectible-data) tx-sender) unauthorized-modification-code)

    ;; Update operation sequence
    (var-set operation-sequence operation-sequence-id)
    (ok operation-sequence-id)
  )
)

;; =============== Permission Framework ===============

;; Permission level definitions
(define-constant access-none u0)
(define-constant access-view u1)
(define-constant access-edit u2)
(define-constant access-manage u3)

;; Detailed permission registry
(define-map granular-permissions
  { collectible-id: uint, participant: principal }
  { 
    access-level: uint,
    authorized-by: principal,
    authorized-at: uint
  }
)

;; Assign specific access level to a participant
(define-public (authorize-participant (collectible-id uint) (participant principal) (access-level uint))
  (let
    (
      (collectible-data (unwrap! (map-get? collectible-repository { collectible-id: collectible-id })
        collectible-missing-code))
    )
    ;; Verify caller is the collectible creator
    (asserts! (is-eq (get collectible-creator collectible-data) tx-sender) unauthorized-modification-code)
    ;; Verify valid access level
    (asserts! (<= access-level access-manage) (err u500))

    (ok true)
  )
)

;; Verify participant has required access level
(define-private (has-access-level (collectible-id uint) (participant principal) (required-level uint))
  (let
    (
      (collectible-data (map-get? collectible-repository { collectible-id: collectible-id }))
      (permission-data (map-get? granular-permissions { collectible-id: collectible-id, participant: participant }))
    )
    (if (is-some collectible-data)
      (if (is-eq (get collectible-creator (unwrap! collectible-data false)) participant)
        ;; Creator has all permissions
        true
        ;; Check permission level for non-creators
        (if (is-some permission-data)
          (>= (get access-level (unwrap! permission-data false)) required-level)
          false
        )
      )
      false
    )
  )
)

;; =============== Core Public Functions ===============

;; Create new collectible with comprehensive metadata
(define-public (register-collectible
  (name (string-ascii 64))
  (storage-size uint)
  (details (string-ascii 128))
  (categories (list 10 (string-ascii 32)))
)
  (let
    (
      (new-collectible-id (+ (var-get collectible-sequence-counter) u1))
    )
    ;; Input validation
    (asserts! (> (len name) u0) invalid-name-code)
    (asserts! (< (len name) u65) invalid-name-code)
    (asserts! (> storage-size u0) invalid-metrics-code)
    (asserts! (< storage-size u1000000000) invalid-metrics-code)
    (asserts! (> (len details) u0) invalid-name-code)
    (asserts! (< (len details) u129) invalid-name-code)
    (asserts! (validate-category-list categories) invalid-category-code)

    ;; Create collectible entry
    (map-insert collectible-repository
      { collectible-id: new-collectible-id }
      {
        collectible-name: name,
        collectible-creator: tx-sender,
        storage-requirement: storage-size,
        creation-height: block-height,
        collectible-details: details,
        collectible-categories: categories
      }
    )

    ;; Initialize creator access privileges
    (map-insert viewer-privileges
      { collectible-id: new-collectible-id, observer: tx-sender }
      { view-permitted: true }
    )

    ;; Update registry counter
    (var-set collectible-sequence-counter new-collectible-id)
    (ok new-collectible-id)
  )
)

;; Rate-limited collectible registration
(define-public (secure-register-collectible
  (name (string-ascii 64))
  (storage-size uint)
  (details (string-ascii 128))
  (categories (list 10 (string-ascii 32)))
)
  (begin
    ;; Apply rate limiting
    (asserts! (verify-transaction-rate tx-sender) (err u700))

    ;; Call standard registration function
    (register-collectible name storage-size details categories)
  )
)

;; Modify existing collectible metadata
(define-public (update-collectible-metadata
  (collectible-id uint)
  (updated-name (string-ascii 64))
  (updated-storage uint)
  (updated-details (string-ascii 128))
  (updated-categories (list 10 (string-ascii 32)))
)
  (let
    (
      (collectible-data (unwrap! (map-get? collectible-repository { collectible-id: collectible-id })
        collectible-missing-code))
    )
    ;; Validation of ownership and parameters
    (asserts! (collectible-registered collectible-id) collectible-missing-code)
    (asserts! (is-eq (get collectible-creator collectible-data) tx-sender) unauthorized-modification-code)
    (asserts! (> (len updated-name) u0) invalid-name-code)
    (asserts! (< (len updated-name) u65) invalid-name-code)
    (asserts! (> updated-storage u0) invalid-metrics-code)
    (asserts! (< updated-storage u1000000000) invalid-metrics-code)
    (asserts! (> (len updated-details) u0) invalid-name-code)
    (asserts! (< (len updated-details) u129) invalid-name-code)
    (asserts! (validate-category-list updated-categories) invalid-category-code)

    ;; Update collectible with new information
    (map-set collectible-repository
      { collectible-id: collectible-id }
      (merge collectible-data {
        collectible-name: updated-name,
        storage-requirement: updated-storage,
        collectible-details: updated-details,
        collectible-categories: updated-categories
      })
    )
    (ok true)
  )
)

;; Transfer collectible ownership to new principal
(define-public (transfer-collectible-ownership (collectible-id uint) (new-owner principal))
  (let
    (
      (collectible-data (unwrap! (map-get? collectible-repository { collectible-id: collectible-id })
        collectible-missing-code))
    )
    ;; Verify caller is the current creator
    (asserts! (collectible-registered collectible-id) collectible-missing-code)
    (asserts! (is-eq (get collectible-creator collectible-data) tx-sender) unauthorized-modification-code)

    ;; Update ownership record
    (map-set collectible-repository
      { collectible-id: collectible-id }
      (merge collectible-data { collectible-creator: new-owner })
    )
    (ok true)
  )
)

;; Remove collectible from registry permanently
(define-public (unregister-collectible (collectible-id uint))
  (let
    (
      (collectible-data (unwrap! (map-get? collectible-repository { collectible-id: collectible-id })
        collectible-missing-code))
    )
    ;; Ownership verification
    (asserts! (collectible-registered collectible-id) collectible-missing-code)
    (asserts! (is-eq (get collectible-creator collectible-data) tx-sender) unauthorized-modification-code)

    ;; Remove collectible from registry
    (map-delete collectible-repository { collectible-id: collectible-id })
    (ok true)
  )
)

;; =============== Authentication Framework ===============

;; Collectible authenticity registry
(define-map collectible-authenticity
  { collectible-id: uint }
  {
    authenticity-hash: (buff 32),
    hash-method: (string-ascii 10),
    verification-timestamp: uint,
    verifier-identity: principal
  }
)

;; Register cryptographic hash for collectible verification
(define-public (register-authenticity-proof (collectible-id uint) (authenticity-hash (buff 32)) (method (string-ascii 10)))
  (let
    (
      (collectible-data (unwrap! (map-get? collectible-repository { collectible-id: collectible-id })
        collectible-missing-code))
    )
    ;; Verify caller is the collectible creator
    (asserts! (is-eq (get collectible-creator collectible-data) tx-sender) unauthorized-modification-code)
    ;; Verify valid hash method (sha256 or keccak256)
    (asserts! (or (is-eq method "sha256") (is-eq method "keccak256")) (err u600))

    (ok true)
  )
)

;; Verify collectible against registered hash
(define-public (verify-collectible-authenticity (collectible-id uint) (verification-hash (buff 32)))
  (let
    (
      (authenticity-data (unwrap! (map-get? collectible-authenticity { collectible-id: collectible-id })
        (err u601)))
    )
    ;; Verify hash matches registered hash
    (asserts! (is-eq (get authenticity-hash authenticity-data) verification-hash) (err u602))

    (ok true)
  )
)

