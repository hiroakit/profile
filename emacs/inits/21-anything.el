;;; anything

(when (require 'anything-startup nil t))
(setq 
 ;; 候補の最大表示数。デフォルトは50
 anything-candidate-number-limit 30
)

;; anything起動用ショートカット
(global-set-key (kbd "\C-x C-x") 'anything)
