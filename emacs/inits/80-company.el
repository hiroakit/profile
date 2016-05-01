
(defun hp-initialize-company-keybinding ()
  "company-modeのキーバインディングを初期化します"
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-h") nil))

(defun hp-initialize-company-face-attribute ()
  "company-modeの配色を初期化します"
  (set-face-attribute 'company-tooltip nil :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common nil :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common-selection nil :foreground "white" :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil :foreground "black" :background "steelblue")
  (set-face-attribute 'company-preview-common nil :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil :background "orange")
  (set-face-attribute 'company-scrollbar-bg nil :background "gray40"))

(when (require 'company nil t)
  (global-company-mode) ; 全バッファで有効にする 
  (setq company-idle-delay 0) ; デフォルトは0.5
  (setq company-minimum-prefix-length 2) ; デフォルトは4
  (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る  
  (hp-initialize-company-face-attribute)
  (hp-initialize-company-keybinding)
  

  (when (require 'irony nil t)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    (add-to-list 'company-backends 'company-irony)))

;; (require 'company-sourcekit)
;; (add-to-list 'company-backends 'company-sourcekit)
