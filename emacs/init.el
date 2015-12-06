;;; ロードパスの設定とinit-loaderを読み込むためのファイル

;;; 定数宣言
(defconst hp-elpa-dir (concat user-emacs-directory "elpa"))
(defconst hp-site-lisp-dir (concat user-emacs-directory "site-lisp"))
(defconst hp-inits-dir (concat user-emacs-directory "inits"))
(defconst hp-org-mode-dir (concat (getenv "HOME") "/src/org-mode/org-8.3.2/lisp"))
(defconst hp-core-conf "~/.emacs.d/init.el")
(defconst hp-org-conf (concat (concat user-emacs-directory "inits") "/30-org.el"))

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

(defun hp-show-core-conf ()
  "init.elを開きます"
  (interactive)
  (switch-to-buffer (find-file-noselect hp-core-conf)))

(defun hp-show-org-conf ()
  "org-modeの設定ファイルを開きます"
  (interactive)
  (switch-to-buffer (find-file-noselect hp-org-conf)))

