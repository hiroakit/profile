;; ruby-mode
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))

;; ruby-electric (ELPAからダウンロードしたものが調子よくない模様)
;; (when (require 'ruby-electric nil t)
;;   (add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
;;   (setq ruby-electric-expand-delimiters-list nil)
;; )

;; ruby-block.el
(when (require 'ruby-block nil t)
  (ruby-block-mode t)
  (setq ruby-block-highlight-toggle t)
)
