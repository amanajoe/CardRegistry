;; CardRegistry: Collectible Trading Card Authentication and Ownership System
;; Version: 1.0.0

(define-constant ERR-NOT-COLLECTOR (err u1))
(define-constant ERR-CARD-NOT-FOUND (err u2))
(define-constant ERR-ALREADY-AUTHENTICATED (err u3))
(define-constant ERR-INVALID-STATUS (err u4))
(define-constant ERR-INVALID-RARITY (err u5))
(define-constant ERR-INVALID-GAME (err u6))
(define-constant ERR-INVALID-EDITION (err u7))
(define-constant ERR-INVALID-NAME (err u8))
(define-constant ERR-INVALID-DESCRIPTION (err u9))

(define-constant MIN-RARITY u1)

(define-data-var next-card-id uint u1)

(define-map trading-cards
    uint
    {
        collector: principal,
        card-name: (string-utf8 50),
        card-description: (string-utf8 180),
        game-series: (string-utf8 12),
        card-edition: (string-utf8 25),
        authentication-status: (string-utf8 18),
        rarity-level: uint
    }
)

(define-private (validate-game (game (string-utf8 12)))
    (or 
        (is-eq game u"Pokemon")
        (is-eq game u"Magic-MTG")
        (is-eq game u"Yu-Gi-Oh")
        (is-eq game u"Baseball")
        (is-eq game u"Basketball")
        (is-eq game u"Football")
    )
)

(define-private (validate-edition (edition (string-utf8 25)))
    (or 
        (is-eq edition u"First-Edition")
        (is-eq edition u"Unlimited")
        (is-eq edition u"Limited-Edition")
        (is-eq edition u"Promotional")
        (is-eq edition u"Tournament")
    )
)

(define-private (validate-text-length (text (string-utf8 180)) (min-length uint) (max-length uint))
    (let 
        (
            (text-length (len text))
        )
        (and 
            (>= text-length min-length)
            (<= text-length max-length)
        )
    )
)

(define-public (authenticate-trading-card 
    (card-name (string-utf8 50))
    (card-description (string-utf8 180))
    (game-series (string-utf8 12))
    (card-edition (string-utf8 25))
    (rarity-level uint))
    (let
        (
            (card-id (var-get next-card-id))
        )
        (asserts! (validate-text-length card-name u3 u50) ERR-INVALID-NAME)
        (asserts! (validate-text-length card-description u10 u180) ERR-INVALID-DESCRIPTION)
        (asserts! (>= rarity-level MIN-RARITY) ERR-INVALID-RARITY)
        (asserts! (validate-game game-series) ERR-INVALID-GAME)
        (asserts! (validate-edition card-edition) ERR-INVALID-EDITION)
        
        (map-set trading-cards card-id {
            collector: tx-sender,
            card-name: card-name,
            card-description: card-description,
            game-series: game-series,
            card-edition: card-edition,
            authentication-status: u"authenticated",
            rarity-level: rarity-level
        })
        (var-set next-card-id (+ card-id u1))
        (ok card-id)
    )
)

(define-public (transfer-card-ownership (card-id uint) (new-collector principal))
    (let
        (
            (trading-card (unwrap! (map-get? trading-cards card-id) ERR-CARD-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get collector trading-card)) ERR-NOT-COLLECTOR)
        (asserts! (is-eq (get authentication-status trading-card) u"authenticated") ERR-INVALID-STATUS)
        (ok (map-set trading-cards card-id (merge trading-card { collector: new-collector, authentication-status: u"transferred" })))
    )
)

(define-read-only (get-trading-card (card-id uint))
    (ok (map-get? trading-cards card-id))
)

(define-read-only (get-card-collector (card-id uint))
    (ok (get collector (unwrap! (map-get? trading-cards card-id) ERR-CARD-NOT-FOUND)))
)