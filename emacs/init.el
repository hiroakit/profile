;;; Emacsの設定

;;; 定数宣言
(defconst hp-elpa-dir (concat user-emacs-directory "elpa"))
(defconst hp-site-lisp-dir (concat user-emacs-directory "site-lisp"))
(defconst hp-inits-dir (concat user-emacs-directory "inits"))
(defconst hp-org-mode-dir (concat (getenv "HOME") "/src/org-mode/org-8.3.2/lisp"))
(defconst hp-core-conf "~/.emacs.d/init.el")
(defconst hp-org-conf (concat (concat user-emacs-directory "inits") "/30-org.el"))
(defvar hp-melpa-url "http://melpa.milkbox.net/packages/")
(defvar hp-marmalade-url "http://marmalade-repo.org/packages/")
(defvar hp-use-package-list
  '(
    ;; 以下に使用するパッケージを記述する
    init-loader
    scala-mode2
    yasnippet
    auto-complete
    foreign-regexp
    web-mode
    js2-mode
    csharp-mode
    cmake-mode
    helm
    ruby-mode
    ruby-additional
    ruby-block
    org-tree-slide
    )
)

;;; パッケージ
(require 'package)
(add-to-list 'package-archives (cons "melpa" hp-melpa-url))
(add-to-list 'package-archives (cons "marmalade" hp-marmalade-url))
(package-initialize)

;;; 未インストールのパッケージを探す
(require 'cl)
(let ((not-installed 
       (loop for x in hp-use-package-list
             when (not (package-installed-p x)) collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist 
        (pkg not-installed)
        (package-install pkg))))

;;; 各パッケージの設定ファイルはinits以下に置く．init-loaderがそれを読み込む
;;; ファイル命名規則が存在する (例 : 10-hoge.el)
(when (require 'init-loader nil t)
  (setq init-loader-show-log-after-init 'error-only)
  (when (file-directory-p (symbol-value 'hp-inits-dir))
    (init-loader-load hp-inits-dir)))
