;;; Code:

(when (require 'flycheck nil t)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  ;;(require 'helm-flycheck)
  (eval-after-load 'flycheck
    '(define-key flycheck-mode-map (kbd "C-+") 'helm-flycheck))

  (add-to-list 'flycheck-checkers 'swift))

;;; 10-flycheck.el ends here
