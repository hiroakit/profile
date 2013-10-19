;; ロードパスの設定とinit-loaderを読み込むためのファイル

(setq load-path
      (append
       (list
        (expand-file-name "~/.emacs.d/")
        (expand-file-name "~/.emacs.d/site-lisp/")
        (expand-file-name "~/.emacs.d/site-lisp/auto-install/")
        (expand-file-name "~/.emacs.d/site-lisp/auto-complete/")
        (expand-file-name "~/.emacs.d/site-lisp/scala-mode2/")
        (expand-file-name "~/.emacs.d/site-lisp/yasnippet/")
       )
       load-path)
)

;; 設定ファイルはinits以下に置く．init-loaderがそれを読み込む
;; 設定ファイルには命名規則が存在する (例 : 10-hoge.el)
(when (require 'init-loader nil t))
(when (file-directory-p "~/.emacs.d/inits")
  (init-loader-load "~/.emacs.d/inits")
)

