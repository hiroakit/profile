;;; ロードパスの設定とinit-loaderを読み込むためのファイル
;;  このファイルは".emacs.d/init.el"として用いること 

(setq load-path
      (append
       (list
        (expand-file-name "~/.emacs.d/")
        (expand-file-name "~/.emacs.d/site-lisp/")
        (expand-file-name "~/.emacs.d/site-lisp/auto-install/")
        (expand-file-name "~/.emacs.d/site-lisp/auto-complete/")
        ;; (expand-file-name "~/.emacs.d/site-lisp/mew/")
        ;; (expand-file-name "~/.emacs.d/site-lisp/org/")

        )
       load-path)
)

;; 設定ファイルはinits以下に置く．init-loaderがそれを読み込む
(when (require 'init-loader nil t))
(when (file-directory-p "~/.emacs.d/inits")
  (init-loader-load "~/.emacs.d/inits")
)

