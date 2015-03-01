;;; ロードパスの設定とinit-loaderを読み込むためのファイル

;;; 定数宣言
(defvar hp-elpa-dir (concat user-emacs-directory "elpa"))
(defvar hp-site-lisp-dir (concat user-emacs-directory "site-lisp"))
(defvar hp-inits-dir (concat user-emacs-directory "inits"))
(defvar hp-org-mode-dir (concat (getenv "HOME") "/src/org-mode/org-8.2.10/lisp"))

;;; 各パッケージのパスをload-pathに展開する関数
(defun hp-expand-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name path)))
      (add-to-list 'load-path default-directory)
      (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;;; load-pathを設定
(when (file-directory-p (symbol-value 'hp-elpa-dir))
  (hp-expand-load-path hp-elpa-dir))
(when (file-directory-p (symbol-value 'hp-site-lisp-dir))
  (hp-expand-load-path hp-site-lisp-dir))
(when (file-directory-p (symbol-value 'hp-org-mode-dir))
  (hp-expand-load-path hp-org-mode-dir))


;;; 各パッケージの設定ファイルはinits以下に置く．init-loaderがそれを読み込む
;;; ファイル命名規則が存在する (例 : 10-hoge.el)
(when (require 'init-loader nil t)
  (when (file-directory-p (symbol-value 'hp-inits-dir))
    (init-loader-load hp-inits-dir)))

