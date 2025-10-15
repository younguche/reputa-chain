;; --------------------------------------------------------------
;;  Decentralized Reputation & Trust Scoring System
;;  Author: [Your Name]
;;  Network: Stacks Blockchain
;;  Purpose: Record and manage decentralized reputation for users
;; --------------------------------------------------------------

;; ------------------------------
;; DATA STRUCTURES & CONSTANTS
;; ------------------------------

(define-constant MAX_SCORE 1000)
(define-constant MIN_SCORE 0)
(define-constant INITIAL_SCORE 500)

;; Store user reputation data
(define-map reputations
  principal
  {
    score: int,
    positive: int,
    negative: int,
    last-updated: uint
  }
)

;; Store individual rating records (to prevent duplicates)
(define-map ratings
  {
    rater: principal,
    rated: principal,
    interaction-id: uint
  }
  int
)

;; ------------------------------
;; ERROR CODES
;; ------------------------------

(define-constant ERR_SELF_RATING (err u100))
(define-constant ERR_DUPLICATE_RATING (err u101))
(define-constant ERR_USER_NOT_FOUND (err u102))
(define-constant ERR_INVALID_RATING (err u103))
(define-constant ERR_NOT_REGISTERED (err u104))

;; ------------------------------
;; EVENTS
;; ------------------------------

(define-map profile-registered
  {
    user: principal
  }
  bool)

(define-map rating-submitted
  {
    rater: principal,
    rated: principal,
    rating: int,
    new-score: int
  }
  bool)

;; ------------------------------
;; PUBLIC FUNCTIONS
;; ------------------------------

;; Register a new user profile
(define-public (register-profile)
  (begin
    (if (is-some (map-get? reputations tx-sender))
        (ok "Profile already exists")
        (begin
          (map-set reputations tx-sender
            {
              score: INITIAL_SCORE,
              positive: 0,
              negative: 0,
              last-updated: u100
            })
          (map-set profile-registered {user: tx-sender} true)
          (ok "Profile registered successfully")
        )
    )
  )
)

;; Submit a rating after an interaction
(define-public (rate-user (rated principal) (interaction-id uint) (rating int))
  (begin
    ;; Ensure a user cannot rate themselves
    (asserts! (not (is-eq rated tx-sender)) ERR_SELF_RATING)

    ;; Ensure valid rating value (+1 or -1)
    (asserts! (or (is-eq rating 1) (is-eq rating -1)) ERR_INVALID_RATING)

    ;; Ensure rated user exists
    (asserts! (is-some (map-get? reputations rated)) ERR_USER_NOT_FOUND)

    ;; Ensure rater has registered
    (asserts! (is-some (map-get? reputations tx-sender)) ERR_NOT_REGISTERED)

    ;; Prevent duplicate ratings for same interaction
    (asserts! (is-none (map-get? ratings {rater: tx-sender, rated: rated, interaction-id: interaction-id}))
              ERR_DUPLICATE_RATING)

    ;; Retrieve profiles
    (let
      (
        (rated-profile (unwrap-panic (map-get? reputations rated)))
        (rater-profile (unwrap-panic (map-get? reputations tx-sender)))
        (rater-score (get score rater-profile))
      )

      ;; Weighted rating influence
      (let
        (
          (weighted (/ (* rating rater-score) 100))
          (new-score (+ (get score rated-profile) weighted))
          (updated-score (if (> new-score MAX_SCORE) MAX_SCORE
                             (if (< new-score MIN_SCORE) MIN_SCORE new-score)))
        )

        ;; Record rating
        (map-set ratings
          {rater: tx-sender, rated: rated, interaction-id: interaction-id}
          rating)

        ;; Update rated user profile
        (map-set reputations rated
          {
            score: updated-score,
            positive: (+ (get positive rated-profile) (if (is-eq rating 1) 1 0)),
            negative: (+ (get negative rated-profile) (if (is-eq rating -1) 1 0)),
            last-updated: u100
          })

        ;; Emit event
        (map-set rating-submitted {rater: tx-sender, rated: rated, rating: rating, new-score: updated-score} true)

        (ok {rated: rated, new-score: updated-score})
      )
    )
  )
)

;; ------------------------------
;; READ-ONLY FUNCTIONS
;; ------------------------------

;; Get a user reputation score
(define-read-only (get-reputation-score (user principal))
  (match (map-get? reputations user)
    profile (ok (get score profile))
    ERR_USER_NOT_FOUND
  )
)

;; Get full profile details
(define-read-only (get-profile (user principal))
  (match (map-get? reputations user)
    profile (ok profile)
    ERR_USER_NOT_FOUND
  )
)

;; Check if rating exists (for verification)
(define-read-only (check-rating (rater principal) (rated principal) (interaction-id uint))
  (map-get? ratings {rater: rater, rated: rated, interaction-id: interaction-id})
)

;; ------------------------------
;; (Optional) ADMIN OR DAO FUNCTIONS
;; ------------------------------

;; Reputation decay (example: reduce score by 1 after long inactivity)
(define-public (apply-decay (user principal))
  (let
    (
      (profile (unwrap! (map-get? reputations user) ERR_USER_NOT_FOUND))
      (inactive-blocks (- u100 (get last-updated profile)))
    )
    (if (>= inactive-blocks u10000)
        (begin
          (map-set reputations user
            {
              score: (- (get score profile) 1),
              positive: (get positive profile),
              negative: (get negative profile),
              last-updated: u100
            })
          (ok "Decay applied"))
        (ok "User still active"))
  )
)
