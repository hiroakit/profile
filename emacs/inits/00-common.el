;; Emacs全体で使うインデントの値を設定
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; スタートアップメッセージを非表示
(setq inhibit-startup-screen t)

;; ファイルを以下の動作で開くときに, 新しいウィンドウで開かず, 新規バッファで開く
;; ドラッグ&ドロップ, あるいは右クリック経由のコンテキストメニューなど
(define-key global-map [ns-drag-file] 'ns-find-file)
(setq ns-pop-up-frames nil)

;; 対応する括弧を光らせる
(show-paren-mode t)

;; バッファを起動時に読み込む
(global-auto-revert-mode 1)

;; 大文字と小文字で区別しない
(setq completion-ignore-case t)

;; symlinkを追いかけるかの問いに, 常にYESと返す
(setq vc-follow-symlinks t)

;; スクロールを一行ずつにする
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)

;; buffer-nameを識別しやすくする設定
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Emacsの配色
(custom-set-faces
 '(default ((t
             (:background "black" :foreground "#CCCCCC")
             )))
 '(cursor ((((class color)
             (background dark))
            (:background "#00AA00"))
           (((class color)
             (background light))
            (:background "#999999"))
           (t ())
           )))
