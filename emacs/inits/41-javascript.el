;; j2-mode

;; ELPAを使っている場合は, autoloadを明示的に設定しなくてよい
;; (autoload 'js2-mode "js2-mode" 
;;   "Mode for editing javascript source files" t)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
          '(lambda ()
             (setq js2-bounce-indent-flag nil)
             (define-key js2-mode-map "\C-m" 
               'newline-and-indent)
             (define-key js2-mode-map "\C-i" 
               'move-cursor-if-needed-when-indentation)
             (add-to-list 'ac-dictionary-files 
                          "~/.emacs.d/dict/ac-user-dict/apple-uiautomation")))
(defun move-cursor-if-needed-when-indentation ()
  (interactive)
  (indent-for-tab-command)
  (let ((point-of-indentation
         (save-excursion
           (back-to-indentation)
           (point))))
    (skip-chars-forward "\s " point-of-indentation)))

(add-hook 'hoge-mode-hook ;; 適用先のモード
  '(lambda ()
    
))
