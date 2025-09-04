;; winning-rfp-brief.clar
;; Short unique Clarity contract for fair RFP in Google Clarity Web3

(clarity-version 2)

;; Data storage
(define-data-var rfp-id uint u0)
(define-map rfps ((id uint))
  ((owner principal) (commit-end uint) (reveal-end uint) (winner (optional principal))))
(define-map commits ((id uint) (vendor principal)) ((h (buff 32))))
(define-map reveals ((id uint) (vendor principal)) ((proposal (string-utf8 64))))

;; Create new RFP
(define-public (create-rfp (commit-end uint) (reveal-end uint))
  (begin
    (asserts! (> commit-end block-height) (err u100))
    (asserts! (> reveal-end commit-end) (err u101))
    (let ((id (+ (var-get rfp-id) u1)))
      (map-set rfps { id: id } { owner: tx-sender, commit-end: commit-end, reveal-end: reveal-end, winner: none })
      (var-set rfp-id id)
      (ok id))))

;; Commit (vendors submit hash of proposal+salt)
(define-public (commit (id uint) (h (buff 32)))
  (begin
    (asserts! (<= block-height (get commit-end (unwrap! (map-get? rfps { id: id }) (err u102)))) (err u103))
    (map-set commits { id: id, vendor: tx-sender } { h: h })
    (ok true)))

;; Reveal (vendors show proposal + salt to verify)
(define-public (reveal (id uint) (proposal (string-utf8 64)) (salt (buff 32)))
  (let ((stored (map-get? commits { id: id, vendor: tx-sender })))
    (match stored
      some-commit (let ((calc (sha256 (concat (utf8-to-bytes proposal) salt))))
                    (asserts! (is-eq (get h some-commit) calc) (err u104))
                    (map-set reveals { id: id, vendor: tx-sender } { proposal: proposal })
                    (ok true))
      none (err u105)))))

;; Finalize winner (owner only, after reveal phase)
(define-public (finalize (id uint) (winner principal))
  (let ((rfp (map-get? rfps { id: id })))
    (match rfp
      some-rfp (begin
                 (asserts! (is-eq (get owner some-rfp) tx-sender) (err u106))
                 (asserts! (> block-height (get reveal-end some-rfp)) (err u107))
                 (map-set rfps { id: id } (merge some-rfp { winner: (some winner) }))
                 (ok winner))
      none (err u108)))))
