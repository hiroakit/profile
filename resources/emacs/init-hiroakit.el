;;; Emacsの設定

(message "Run init.el")

;; 定数宣言
(defconst hp-core-conf (concat user-emacs-directory "init.el"))
(defconst hp-elpa-dir (concat user-emacs-directory "elpa"))
(defconst hp-site-lisp-dir (concat user-emacs-directory "site-lisp"))
(defconst hp-org-mode-dir (concat (getenv "HOME") "/src/org-mode/org-9.1.12/lisp"))
(defconst hp-org-mode-local-config-file (concat (file-name-as-directory hp-site-lisp-dir) "hp-org-mode-local-config.el"))
(defconst hp-default-emacs-frame-width-size 120 "フレームの横幅の初期値")
(defconst hp-default-tab-space-length 4 "タブを半角スペースで置き換える際の文字数")
(defvar hp-use-package-list '(
    ;; 以下に使用するパッケージを記述する
    cmake-mode
    company
    company-irony
    dired-sidebar
    flycheck
    foreign-regexp
    helm
    helm-gtags
    helm-swoop
    irony
    js2-mode
    org
    osx-dictionary
    ruby-mode
    ruby-additional
    swift-mode
    undo-tree
    undohist
    web-mode
    yasnippet
    zoom-window))

;;; ライブラリの読み込み

(require 'cl)      ;; common lisp
(require 'server)  ;; emacsのserver-startを呼ぶために使う

;;; TYPEFACE CONFIGURATIONS

;; (defconst hiroakit/typeface-default-family "Myrica M"
;;   "The default typeface.")
;; (defconst hiroakit/typeface-default-fontsize "12"
;;   "The default typeface size.")

(defconst hiroakit/typeface-default-family "Migu 1M"
  "The default typeface.")
(defconst hiroakit/typeface-default-fontsize "12"
  "The default typeface size.")

(defun hiroakit/typeface (family height)
  "Set default typeface using family and height"
  (when (member family (font-family-list))
    (message "hiroakit/typeface: %s founded." family)
    (set-face-attribute 'default nil :family family :height height)
    (set-face-attribute 'fixed-pitch nil :family family :height height)))

;; (use-package emacs
;;   :config
;;   (defconst prot/variable-pitch-font "DejaVu Sans"
;;     "The default variable-pitch typeface.")

;;   (set-face-attribute 'variable-pitch nil :family prot/variable-pitch-font :height 1.0)
;;   (set-face-attribute 'fixed-pitch nil :family prot/fixed-pitch-font :height 1.0))

;; (set-face-attribute 'default nil :family "Menlo" :height 120)
;; (set-fontset-font (frame-parameter nil 'font)
;;                   'japanese-jisx0208
;;                   (font-spec :family "Hiragino Kaku Gothic ProN"))
;; (add-to-list 'face-font-rescale-alist
;;              '(".*Hiragino Kaku Gothic ProN.*" . 1.2))

;; Intractive functions
(defun hiroakit/typeface-laptop ()
  "Fonts for the small laptop screen. Pass desired argument to `prot/font-sizes' for use on my small laptop monitor."
  (interactive)
  (when window-system
    (hiroakit/typeface hiroakit/typeface-default-family 105)))

(defun hiroakit/typeface-screencast ()
  "Fonts for screen casts and video demos.
   Pass desired argument to `prot/font-sizes' for use during screencasting. 
   The idea is to make it easier for viewers to see what I am doing."
  (interactive)
  (when window-system
    (hiroakit/typeface hiroakit/typeface-default-family 160)))

(defun hiroakit/typeface-presentation ()
  "Fonts for presentations and video blogs.
   Pass desired argument to `prot/font-sizes' for use during presentations.  
   Also see `prot/org-presentation'."

  (interactive)
    (when window-system
    (hiroakit/typeface hiroakit/typeface-default-family 180)))

;; Key-bindings
(global-set-key (kbd "C-<wheel-up>")
                '(lambda()
                   (interactive)
                   (text-scale-increase 1)))
(global-set-key (kbd "C-<wheel-down>")
                '(lambda()
                   (interactive)
                   (text-scale-decrease 1)))
(global-set-key (kbd "M-0")
                '(lambda()
                   (interactive)
                   (text-scale-set 0)))

;;; フォント設定
(defun hp-load-font-config ()
   "フォントに関する設定をする. プライベートな関数として扱うこと."

   (add-to-list 'default-frame-alist '(font . "Migu 1M 15"))
   (hiroakit/typeface hiroakit/typeface-default-family 100)

;;     (add-to-list 'default-frame-alist '(font . "Migu 1M 12")))
;;     (add-to-list 'default-frame-alist '(font . (hiroakit/typeface))))

     ;; (add-to-list 'default-frame-alist
     ;;              '(font . (format "%s %s"
     ;;                               (symbol-value 'hiroakit/default-typeface)
     ;;                               (symbol-value 'hiroakit/default-typeface-size)))))
  ;; (defvar hp-font-size 12 "フォントサイズの初期値を返します")
  ;; (defun my-ja-font-setter (spec)
  ;;   (set-fontset-font nil 'japanese-jisx0208 spec)
  ;;   (set-fontset-font nil 'katakana-jisx0201 spec)
  ;;   (set-fontset-font nil 'japanese-jisx0212 spec)
  ;;   (set-fontset-font nil '(#x0080 . #x024F) spec)
  ;;   (set-fontset-font nil '(#x0370 . #x03FF) spec)
  ;;   (set-fontset-font nil 'mule-unicode-0100-24ff spec))
  
  ;; (defun my-ascii-font-setter (spec)
  ;;   (set-fontset-font nil 'ascii spec))

  ;; (when (not window-system)
  ;;   (when (equal emacs-major-version 26)
      
  ;;     ;; (let
  ;;     ;;     ;; 1) Monaco, Hiragino/Migu 2M : font-size=12, -apple-hiragino=1.2
  ;;     ;;     ;; 2) Inconsolata, Migu 2M     : font-size=14, 
  ;;     ;;     ;; 3) Inconsolata, Hiragino    : font-size=14, -apple-hiragino=1.0
  ;;     ;;     ((font-size hp-font-size) (ascii-font "MigMix 2M") (ja-font "MigMix 2M"))
  ;;     ;;   (my-ascii-font-setter (font-spec :family ascii-font :size font-size))
  ;;     ;;   (my-ja-font-setter (font-spec :family ja-font :size font-size)))
      
  ;;     (set-default 'line-spacing 1) ;; Space between lines
  ;;     (setq mac-allow-anti-aliasing t) ;; Anti aliasing with Quartz 2D
  ;;     ))
  
  ;; (cond
  ;;  ;; CocoaEmacs
  ;;  ((eq window-system 'ns)
  ;;   (when (equal emacs-major-version 26)
      
  ;;   ;; (when (or (= emacs-major-version 24) (= emacs-major-version 25))
  ;;     (let
  ;;         ;; 1) Monaco, Hiragino/Migu 2M : font-size=12, -apple-hiragino=1.2
  ;;         ;; 2) Inconsolata, Migu 2M     : font-size=14, 
  ;;         ;; 3) Inconsolata, Hiragino    : font-size=14, -apple-hiragino=1.0
  ;;         ((font-size hp-font-size)
  ;;          (ascii-font "Migu 1M")
  ;;          (ja-font "Migu 1M"))
  ;;       (my-ascii-font-setter (font-spec :family ascii-font :size font-size))
  ;;       (my-ja-font-setter (font-spec :family ja-font :size font-size)))
      
  ;;     ;; Fix ratio provided by set-face-attribute for fonts display
  ;;     (setq face-font-rescale-alist
  ;;           '(("^-apple-hiragino.*" . 1.0) ; 1.2
  ;;             (".*Migu.*" . 1.2)
  ;;             (".*Inconsolata.*" 1.0)
  ;;             (".*osaka-bold.*" . 1.0)     ; 1.2
  ;;             (".*osaka-medium.*" . 1.0)   ; 1.0
  ;;             (".*courier-bold-.*-mac-roman" . 1.0) ; 0.9
  ;;             ("-cdac$" . 1.0)))           ; 1.3
  ;;     ;; Space between lines
  ;;     (set-default 'line-spacing 1)
  ;;     ;; Anti aliasing with Quartz 2D
  ;;     (setq mac-allow-anti-aliasing t)))
  ;;  ((eq window-system 'w32)
  ;;   ;; (let
  ;;   ;;     (
  ;;   ;;      (font-size 24)
  ;;   ;;      (ascii-font "MigMix 1M")
  ;;   ;;      (ja-font "MigMix 1M")
  ;;   ;;     )
  ;;   ;;   (my-ascii-font-setter (font-spec :family ascii-font :size font-size))
  ;;   ;;   (my-ja-font-setter (font-spec :family ja-font :size font-size)))
    
  ;;   ;; default        : デフォルトフォント
  ;;   ;; variable-pitch : プロポーショナルフォント
  ;;   ;; fixed-pitch    : 等幅フォント
  ;;   ;; tooltip        : ツールチップ用フォント
  ;; (set-face-attribute 'default nil :family "Migu 1M" :height 100)
  ;; (set-face-attribute 'variable-pitch nil :family "Migu 1M" :height 100)
  ;; (set-face-attribute 'fixed-pitch nil :family "Migu 1M" :height 100)
  ;; (set-face-attribute 'tooltip nil :family "Migu 1M" :height 90)
  
  ;; ;; Fix ratio provided by set-face-attribute for fonts display
  ;; (setq face-font-rescale-alist '((".*Migu.*" . 1.0)))
  
  ;; Space between lines
  (set-default 'line-spacing 1))                                       

;;; フェイス設定
(defun hp-load-faces-config ()  
  (custom-set-faces
   ;; M-x describe-face       => カーソルが当たっている箇所のフェイスを表示する
   ;; M-x list-faces-display  => 現在の設定をバッファに表示する
   ;; M-x list-colors-display => 色を一覧表示する
   ;;
   ;; #D6EAF8 ライトブルー (紫に近い)
   ;; #0062A0 コバルトブルー
   ;; #f1f1f1 スノーホワイト
   
   '(default     ((t (:inherit nil :stipple nil :background nil :foreground "gray0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "MigMix 2M"))))
   '(bold        ((t (:bold t)))) ;; どこで使用されているのか把握できていない
   '(italic      ((t (:italic t)))) ;; どこで使用されているのか把握できていない
   '(bold-italic ((t (:bold t :italic t)))) ;; どこで使用されているのか把握できていない
   '(underline   ((t (:underline t)))) ;; どこで使用されているのか把握できていない
   '(highlight   ((t (:foreground "gray0" :background "#D6EAF8" :bold t)))) ;; カーソルがある行に帯を出す foregroundはhelm-swoopの検索結果をハイライトすること考慮している
   ;;(setq hl-line-face 'underline)
   
   ;; 選択範囲
   '(region ((t (:background "#D6EAF8")))) 

   ;; paren
   '(show-paren-match ((t (:background "Yellow"))))

   ;; mode-line
   '(mode-line          ((t (:foreground "#ffffff" :background "#0062A0")))) ;; アクティブ時
   '(mode-line-inactive ((t (:foreground "#000000" :background "#f1f1f1")))) ;; 非アクティブ時
   '(minibuffer-prompt  ((t (:foreground "#0062A0" :bold t))))

   ;; isearch
   '(isearch ((t (:foreground nil :background "Yellow" :bold t))))
   '(lazy-highlight ((t (:foreground nil :background "light yellow" :bold nil))))
   
   ;; dired-mode
   '(dired-header    ((t (:foreground nil :background nil :bold t)))) ;; ディレクトリパス
   '(dired-directory ((t (:foreground "Blue1" :bold nil)))) ;; ディレクトリ名
   '(dired-symlink   ((t (:foreground "Purple" :bold nil)))) ;; シンボリックリンク
   '(dired-mark      ((t (:foreground "dark cyan" :bold t)))) ;; 項目選択時に行頭に表示される「*」
   '(dired-marked    ((t (:inherit dired-mark)))) ;; 項目選択時のファイル名、ディレクトリ名

   ;; helm-mode
   '(helm-source-header                  ((t (:foreground nil :background "#f1f1f1" :bold t))))      
   '(helm-visible-mark                   ((t (:inherit highlight))))
   '(helm-selection                      ((t (:inherit highlight))))
   '(helm-selection-line                 ((t (:inherit highlight))))
   '(helm-ff-directory                   ((t (:inherit dired-directory))))
   '(helm-ff-dotted-directory            ((t (:inherit helm-ff-directory))))
   '(helm-bookmark-directory             ((t (:inherit helm-ff-directory))))
   '(helm-buffer-directory               ((t (:inherit helm-ff-directory))))
   '(helm-ff-file                        ((t (:inherit default))))
   '(helm-bookmark-file                  ((t (:inherit helm-ff-file))))
   '(helm-buffer-file                    ((t (:inherit helm-ff-file))))
   '(helm-grep-file                      ((t (:inherit helm-ff-file))))
   '(helm-etags-file                     ((t (:inherit helm-ff-file))))
   '(helm-ff-executable                  ((t (:inherit helm-ff-file))))
   '(helm-ff-symlink                     ((t (:inherit dired-symlink))))
   '(helm-ff-dotted-symlink-directory    ((t (:inherit helm-ff-symlink))))
   '(helm-ff-truename                    ((t (:inherit helm-ff-symlink))))
   '(helm-ff-invalid-symlink             ((t (:inherit error))))

   ;; company-mode
   (with-eval-after-load 'company-mode      
     (set-face-attribute 'company-tooltip nil :foreground "black" :background "lightgrey")
     (set-face-attribute 'company-tooltip-common nil :foreground "black" :background "lightgrey")
     (set-face-attribute 'company-tooltip-common-selection nil :foreground "white" :background "steelblue")
     (set-face-attribute 'company-tooltip-selection nil :foreground "black" :background "steelblue")
     (set-face-attribute 'company-preview-common nil :background nil :foreground "lightgrey" :underline t)
     (set-face-attribute 'company-scrollbar-fg nil :background "orange")
     (set-face-attribute 'company-scrollbar-bg nil :background "gray40"))

   ;; org-mode
   (with-eval-after-load 'org-mode      
     (set-face-attribute 'org-block nil :foreground "black") ;; #+BEGIN_SRC - #+END_SRCの装飾
     (setq org-src-block-faces '(("emacs-lisp" (:background "#EEE2FF"))
                                 ("python" (:background "#E5FFB8"))))
     (setq org-todo-keyword-faces
           '(("TODO"     . org-warning)
             ("CANCELED" . shadow)))) ;; Taskの属性名につける装飾
   )
)

;;; キーバインディング設定
(defun hp-load-key-binding-config ()
  ;;(when (not window-system)
  ;; Emacs全般
  (global-set-key (kbd "C-x j") 'goto-line)

  ;; macOSのEmacs.app向け
  (when (equal window-system 'ns)
    ;; C-zを無効にする
    (global-unset-key "\C-z")

    ;; \C-+ で拡大
    (global-set-key [(control ?+)] (lambda () (interactive) (text-scale-increase 1)))
    ;; \C-- で縮小
    (global-set-key [(control ?-)] (lambda () (interactive) (text-scale-decrease 1)))
    ;; \C-0 でデフォルトに戻す
    (global-set-key [(control ?0)] (lambda () (interactive) (text-scale-increase 0)))  
    ) 
  
  ;; org-mode
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c b") 'org-iswitchb)

  ;; frameの境界線を動かす
  (define-key global-map (kbd "C-c C-r") 'hp-move-frame-line)
  
  ;; コメントアウトのキーバインディングををCtrl押しながらに変更する
  (define-key global-map (kbd "M-;") nil) 
  (define-key global-map (kbd "C-;") 'comment-dwim) 
  
  ;; 文字コードと改行コードの変更する関数
  (define-key global-map (kbd "C-c C-e") 'set-buffer-file-coding-system) 
  
  ;; 矩形範囲の拡大・縮小
  (global-set-key (kbd "C-c r u") 'er/expand-region)
  (global-set-key (kbd "C-c r d") 'er/contract-region)
  
  ;; 補完の起動
  (global-set-key (kbd "C-c .") 'company-complete)

  ;; 日時の取得
  (global-set-key (kbd "C-c C-d c") 'hp-insert-current-date-text)
  (global-set-key (kbd "C-c C-d y") 'hp-insert-current-year-text)
  (global-set-key (kbd "C-c C-d m") 'hp-insert-current-month-text)
  (global-set-key (kbd "C-c C-d d") 'hp-insert-current-day-text)
  
  ;; 一時的なorgのバッファを作成
  (global-set-key (kbd "C-c t") 'hp-create-temp-org-buffer)
  
  ;; 現在のディレクトリをFinderで開く
  (global-set-key (kbd "C-c f") 'hp-open-current-directory)
  
  ;; フレームサイズの調整
  (global-unset-key (kbd "C-x ^"))
  (global-unset-key (kbd "<C-x {>"))
  (global-unset-key (kbd "<C-x }>"))  
  (global-set-key (kbd "<C-S-up>") 'enlarge-window)
  (global-set-key (kbd "<C-S-down>") 'shrink-window)
  (global-set-key (kbd "<C-S-right>") 'enlarge-window-horizontally)
  (global-set-key (kbd "<C-S-left>") 'shrink-window-horizontally)
  
  ;; バッファの切り替え
  (global-unset-key (kbd "C-x <left>"))  ;; 初期値
  (global-unset-key (kbd "C-x <right>")) ;; 初期値
  (global-set-key (kbd "C-x {") 'previous-buffer)
  (global-set-key (kbd "C-x }") 'next-buffer)  

  ;; バッファ内の移動
  (global-unset-key (kbd "<M-<>")) ;; 初期値
  (global-unset-key (kbd "<M->>")) ;; 初期値
  (global-set-key (kbd "C-x <left>") 'beginning-of-buffer)
  (global-set-key (kbd "C-x <right>") 'end-of-buffer)

  ;; バッファの保存、再読み込み
  (global-set-key (kbd "C-x C-s") 'save-buffer)
  (global-set-key (kbd "C-x C-r") 'eval-buffer)

  ;; hs-minor-mode
  (global-set-key (kbd "C-c /") 'hs-toggle-hiding)
  
  ;; helm-mode
  (with-eval-after-load 'helm-mode      
    (define-key helm-map            (kbd "C-h") 'delete-backward-char)
    (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
    (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
    (define-key helm-read-file-map  (kbd "TAB") 'helm-execute-persistent-action)
;;    (define-key global-map (kbd "M-x")     'helm-M-x)            ;; Emacsのコマンドを絞り込むためのキーバインディグ
    (define-key global-map (kbd "C-x C-f") 'helm-find-files)     ;; ファイル探す際に絞り込むためのキーバインディグ
    ;; (define-key global-map (kbd "C-x C-r") 'helm-recentf)     ;; 最近開いたファイルを絞り込むためのキーバインディグ
    (define-key global-map (kbd "M-y")     'helm-show-kill-ring) ;; キリングを絞り込むためのキーバインディング
    (define-key global-map (kbd "C-c i")   'helm-imenu)          ;; バッファ内に存在する関数を絞り込むためのキーバインディグ
    (define-key global-map (kbd "C-x C-b") 'helm-buffers-list)   ;; バッファを絞り込むためのキーバインディグ
    (define-key global-map (kbd "C-x C-x") 'helm-for-files)      ;; 絞り込み対象をとても広く取ってから絞り込むためのキーバインディグ
    (define-key global-map (kbd "C-x C-m") 'helm-mini)           ;; 絞り込み対象をやや広く取ってから絞り込むためのキーバインディグ
    (define-key global-map (kbd "M-r")     'helm-resume)         ;; 前回のhelmコマンドの続きから絞り込むためのキーバインディグ
    (global-set-key (kbd "C-x g g") 'helm-ag)                    ;; 
    )

  (with-eval-after-load 'helm-gtags
    ;; (setq helm-gtags-suggested-key-mapping t)
    ;; (setq helm-gtags-prefix-key "\C-c")
    
    ;; helm-gtags-find-tag               関数の定義場所の検索
    ;; helm-gtags-find-rtag              関数や使用箇所の検索
    ;; helm-gtags-find-symbol            変数の使用箇所の検索
    ;; helm-gtags-pop-stack              タグジャンプした箇所からひとつ戻る
    ;; helm-gtags-parse-file             関数の定義一覧
    ;; helm-gtags-tags-in-this-function  関数内のタグ一覧
    (define-key helm-gtags-mode-map (kbd "C-c C-t") 'helm-gtags-find-tag)
    (define-key helm-gtags-mode-map (kbd "C-c C-r") 'helm-gtags-find-rtag)
    (define-key helm-gtags-mode-map (kbd "C-c C-s") 'helm-gtags-find-symbol)
    (define-key helm-gtags-mode-map (kbd "C-c C-p") 'helm-gtags-pop-stack)
    (define-key helm-gtags-mode-map (kbd "C-c C-f") 'helm-gtags-parse-file)
    (define-key helm-gtags-mode-map (kbd "C-c C-a") 'helm-gtags-tags-in-this-function)
    )

  (with-eval-after-load 'company-mode
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (define-key company-active-map (kbd "C-h") nil))

  (add-hook 'org-mode-hook
            (lambda ()
              "Custmize org-mode key binding."

              (message "Run org-mode-hook at not window-system")

              ;; org-modeのlocal-mapよりもglobal-mapを優先させる。
              ;; フレームサイズの調整が楽な方が嬉しいから。
              (local-unset-key (kbd "<C-S-up>"))
              (local-unset-key (kbd "<C-S-down>"))
              (local-unset-key (kbd "<C-S-right>"))
              (local-unset-key (kbd "<C-S-left>"))
              ;;(local-set-key (kbd "C-c c") 'hp-show-org-conf)

              ;; TABとC-iは同じなので
              ;; - org-clock-outのキーバインドはC-c C-x C-o
              ;; - org-clock-inのキーバインドはC-c C-x TAB
              ;;
              ;; In ASCII, C-i and <TAB> are the same character.
              ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Function-Keys.html
              ;;
              ;; だから以下の設定は無意味
              ;; (local-unset-key (kbd "C-c C-x TAB"))
              ;; (local-set-key (kbd "C-c C-x C-i") 'org-clock-in)            
              ))

  (add-hook 'gdb-mode-hook
            (lambda ()
              "Custmize c-mode"

              (setq gdb-many-windows t)

              ;;; I/O バッファを表示
              (setq gdb-use-separate-io-buffer t)

              ;;; t にすると mini buffer に値が表示される
              (setq gud-tooltip-echo-area nil)

              (gud-tooltip-mode t)
              ))
  )

;;; Emacs初期化
;;
;; Emacs初期化のライフサイクル
;; 1. Run before-init-hook
;; 2. Run loading init files
;; 3. Run after-init-hook
;; 4. Run emacs-startup-hook

(defun hp-run-emacs-initialization ()
  "Emacsを画面に表示する前に適用したい設定を集めた。プライベートな関数として扱うこと。"

  ;; (setq debug-on-error t)
  (setq inhibit-startup-screen t)   ;; 起動時のスプラッシュイメージを表示しない
  (setq initial-scratch-message "") ;; scratchの初期メッセージを表示しない
  (setq ring-bell-function 'ignore) ;; 警告音もフラッシュも全て無効
  (tool-bar-mode -1)                ;; ツールバーを表示しない
  (menu-bar-mode -1)                ;; メニューバーを表示しない  
  (set-scroll-bar-mode nil)         ;; スクロールバーを表示しない
  (hp-load-font-config)             ;; フォントに関する設定を読み込む.
  (hp-load-faces-config)            ;; フェイスの設定を読み込む
  (hp-load-key-binding-config)      ;; キーバインディングの設定を読み込む
)

(defun hp-emacs-after-init ()
  "Emacs初期化後に適用する設定。プライベートな関数として扱うこと。"
  
  (hp-load-load-path-config) ;; load-pathを設定
  (when (require 'expand-region nil t)) ;; 矩形の拡大・縮小
  (when (require 'hp-utility nil t)) ;; 自作のユーティリティツールを読み込む.  
  (when (require 'uniquify nil t) (setq uniquify-buffer-name-style 'post-forward-angle-brackets)) ;; buffer-nameを識別しやすくする設定
  (setq frame-title-format (format "%%f @%s" (system-name))) ;; フレームのタイトルバーにファイルのフルパスとホスト名を表示する.
  (line-number-mode t)   ;; モードラインに行番号表示
  (column-number-mode t) ;; モードラインに列番号表示
  (setq ns-pop-up-frames nil) ;; ファイルを開くときには新しいウィンドウで開かずに新規バッファで開く
  (setq vc-follow-symlinks t) ;; symlinkを追いかけるかの問いに, 常にYESと返す
  (setq scroll-conservatively 35 scroll-margin 0 scroll-step 1) ;; スクロールを1行毎にする  
  (setq completion-ignore-case t) ;; 大文字と小文字を区別しない
  ;; (define-key global-map [ns-drag-file] 'ns-find-file) ;; ドラッグ&ドロップ, あるいは右クリック経由のコンテキストメニューなど
  (setq-default indent-tabs-mode nil) ;; タブで字下げする場合に半角スペースを利用する
  (setq-default tab-width hp-default-tab-space-length)
  
  

  ;; 括弧の範囲内を強調表示
  (show-paren-mode t)
  (setq show-paren-delay 0)
  (setq show-paren-style 'parenthesis)   

  (when (require 'undohist nil t) (undohist-initialize)) ;; undohist undoの履歴をウィンドウを閉じても保持する
  (when (require 'undo-tree nil t) (global-undo-tree-mode)) ;; undo-tree undoの樹形図を表示する C-x u
  
  ;; 矩形選択にcua-modeを使う.
  (cua-mode t)
  (setq cua-enable-cua-keys nil)
    
  (global-auto-revert-mode 1) ;; ファイルを読み込み直す revert-buffer の自動実行を、すべてのメジャーモードにおいて許可する.

  (add-hook 'emacs-lisp-mode-hook 'hp-emacs-lisp-mode-hook) ;; emacs-lisp-modeの設定

  ;; Get $PATH from SHELL
  (when (require 'exec-path-from-shell)
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize)))

  )

(defun hp-emacs-startup ()
  (modify-frame-parameters nil (list (cons 'width hp-default-emacs-frame-width-size))) ;; フレームサイズを調整する.
  (hp-load-helm-config)            ;; helmの設定            
  (hp-load-org-mode-config)        ;; org-modeの設定
  (hp-load-org-agenda-mode-config) ;; org-agenda-modeの設定
  ;; (hp-load-auto-install-config) ;; auto-installの設定
  ;; (hp-load-cmake-mode-config)    ;; cmake-modeの設定          
  ;; (hp-load-flycheck-config)     ;; flycheckの設定
  ;; (hp-load-yas-config)          ;; yasの設定
  ;; (hp-load-ruby-mode-config)    ;; ruby-modeの設定
  ;; (hp-load-js2-mode-config)     ;; js2-modeの設定 
  ;; (hp-load-web-mode-config)     ;; web-modeの設定
  ;; (hp-load-company-mode-config) ;; company-modeの設定
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
)

;; Emacsの初期化時に指定したい設定
(hp-run-emacs-initialization)

;; Emacsの初期化が始まる前のフック
(add-hook 'before-init-hook
          (lambda ()
            (message "Run before-init-hook")))

;; Emacsの初期化が終わった時のフック
(add-hook 'after-init-hook
          (lambda ()
            (message "Run after-init-hook")
            (hp-emacs-after-init)))

;; Emacsが立ち上がった時のフック (after-init-hookよりあとのフック)
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Run emacs-startup-hook")
            (hp-emacs-startup)))                          

;;; パッケージ
;;
;; package.elはEmacsに同梱されている
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;; 未インストールのパッケージを探してインストールする
(let ((not-installed-packages (loop for pkg in hp-use-package-list
                                    unless (package-installed-p pkg) collect pkg)))
  (when not-installed-packages
    (package-refresh-contents)
    (dolist (pkg not-installed-packages)
        (package-install pkg))))


;;; Install for leaf.el
;; (eval-and-compile
;;   (customize-set-variable
;;    'package-archives '(("org" . "https://orgmode.org/elpa/")
;;                        ;;("melpa" . "https://melpa.org/#/")
;;                        ("gnu" . "https://elpa.gnu.org/packages/")))
;;   (package-initialize)
;;   (unless (package-installed-p 'leaf)
;;     (package-refresh-contents)
;;     (package-install 'leaf))

;;   (leaf leaf-keywords
;;     :ensure t
;;     :init
;;     ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
;;     (leaf hydra :ensure t)
;;     (leaf el-get :ensure t)
;;     (leaf blackout :ensure t)

;;     :config
;;     ;; initialize leaf-keywords.el
;;     (leaf-keywords-init)))

(leaf leaf-keywords
  :ensure t
  :init
  ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
  ;; (leaf hydra :ensure t)
  ;; (leaf el-get :ensure t)
  ;; (leaf blackout :ensure t)
  :config  
  (leaf-keywords-init))
(leaf leaf-tree :ensure t)
(leaf leaf-convert :ensure t)
;; (leaf transient-dwim
;;   :ensure t
;;   :bind (("M-=" . transient-dwim-dispatch)))

(defun hiroakit-php-mode-setup ()
  "hiroakit PHP-mode hook."
  (subword-mode 1)
  (setq show-trailing-whitespace t)

  (setq-local page-delimiter "\\_<\\(class\\|function\\|namespace\\)\\_>.+$")

  (require 'flycheck-phpstan)
  (flycheck-mode t)
  (add-to-list 'flycheck-disabled-checkers 'php-phpmd)
  (add-to-list 'flycheck-disabled-checkers 'php-phpcs))

(leaf php-mode
  :hook
  ((php-mode-hook . hiroakit-php-mode-setup))
  :config
  ;;(php-manual-url 'ja)
  ;;(php-mode-coding-style 'psr2)
  ;;(php-project-auto-detect-etags-file . t)
  ;;(php-mode-template-compatibility nil)
  (bind-key "[" (smartchr "[]" "array()" "[[]]") php-mode-map)
  (bind-key "]" (smartchr "array " "]" "]]")     php-mode-map)
  ;; (bind-key "C-}" 'cedit-barf php-mode-map)
  ;; (bind-key "C-)" 'cedit-slurp php-mode-map)
  (bind-key "C-c C-c" 'psysh-eval-region         php-mode-map)
  (bind-key "<f6>" 'phpunit-current-project      php-mode-map)
  (bind-key "C-c C--" 'php-current-class php-mode-map)
  (bind-key "C-c C-=" 'php-current-namespace php-mode-map))

(defun hp-expand-load-path (&rest paths)
  "各パッケージのパスをload-pathに展開する."
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name path)))
      (add-to-list 'load-path default-directory)
      (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

(defun hp-load-load-path-config ()
  "load-pathに関する設定を読み込む. プライベート関数として扱うこと."
  (when (file-directory-p (symbol-value 'hp-elpa-dir))
    (hp-expand-load-path hp-elpa-dir))
  (when (file-directory-p (symbol-value 'hp-site-lisp-dir))
    (hp-expand-load-path hp-site-lisp-dir))
  (when (file-directory-p (symbol-value 'hp-org-mode-dir))
    (hp-expand-load-path hp-org-mode-dir)))

(defun hp-load-helm-config ()
  "helmに関する設定を読み込む. プライベートな関数として扱うこと."  

  ;; helm-modeの設定を読み込む.
  (when (require 'helm-config nil t)
    (let ((ad-redefinition-action 'accept)) (helm-mode 1)))

  ;; helm-ag or ripgrep
  (setq helm-ag-base-command "rg -S --vimgrep --no-heading") 
  
  ;; helm-swoop
  ;; (autoload 'helm-swoop "helm-swoop" "" t)
  (when (require 'helm-swoop nil t))
  
  ;; helm-gtags-mode
  (autoload 'helm-gtags-mode "helm-gtags" "" t))
  
(defun hp-load-org-mode-config ()
  "org-modeに関する設定を読み込む. プライベートな関数として扱うこと."

  ;; Emacsにバンドルされていないorg-mode, 最新版のorg-modeを使う場合は (require org-install) で読み込む.
  ;;
  ;; Running the latest version of org-mode
  ;; http://orgmode.org/worg/org-tutorials/org4beginners.html
  (when (require 'org-install nil t)

  (defun hp-org-load ()
    (setq truncate-lines nil) ;; org-modeではテキストを折り返す
    (setq truncate-partial-width-windows nil)
    (add-to-list 'auto-mode-alist '("\\.org$" . org-mode)) ;; ファイルの拡張子が org だった場合，org-modeを起動するよう登録する.
    (setq org-directory "~/Documents/sources/notebook") ;; orgファイルを格納するディレクトリ.
    (setq org-default-notes-file (concat (file-name-as-directory org-directory) "inbox.org"))  ;; org-default-notes-fileのファイル名.    
    (when (file-exists-p hp-org-mode-local-config-file) (load hp-org-mode-local-config-file))) ;; この環境固有のorg-mode設定を読み込む
     
  ;; org-modeのhookについては下記が詳しい.
  ;; http://orgmode.org/tmp/worg/org-configs/org-hooks.html
  
  ;; org-load-hook (org.elが読み込まれた)
  (add-hook 'org-load-hook
            (lambda ()
              (message "Run org-load-hook at hp-load-org-mode-config")
              (hp-org-load)
              ))
    
  ;; org-mode-hook (org-modeが起動した)    
  (add-hook 'org-mode-hook	      
 	        (lambda ()
              (message "Run org-mode-hook at hp-load-org-mode-config")

              (setq org-startup-truncated nil)  ;; orgファイルは折り畳んだ状態で開く.
              (setq org-hide-leading-stars t)   ;; 必要最低限の「*」のみ表示する (効果はorgファイルに #+STARTUP: hidestars と記述した場合と同じ)
              (setq org-src-fontify-natively t) ;; コードハイライト

              (add-to-list 'org-src-lang-modes '("csharp" . csharp))
              
              ;; TODOステータス (C-c C-tでミニバッファが開く)
              (setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d!)") (sequence "WAITING(w@/!)" "|") (sequence "|" "CANCELED(c@/!)")))

              ;; タグ
              (setq org-tag-alist '(("@HOME" . ?h)
                                    ("@OFFICE" . ?o)
                                    ("@TSUTAYA")
                                    ("@ITOYOKADO" . ?i)
                                    ("@CLEANERS" . ?x)
                                    ("@POSTOFFICE" .?y)
                                    ("@BANK" . ?b)
                                    ("@LUNCHTIME")
                                    ("PHONE" . ?p)
                                    ("MAIL" . ?m)
                                    ("READING" . ?r)
                                    ("WATCHING")
                                    ("CONFERENCE" . ?c)
                                    ("TALKING" . ?t)
                                    ("Scheduling" . ?s)
                                    ("Writting" . ?w)
                                    ("Payment")))
              (setq org-global-properties (quote (("Effort_ALL" . "00:10 00:15 00:30 01:00 01:30 02:00 03:00 04:00 08:00 16:00"))))
              (setq org-columns-default-format "%3PRIORITY(P) %80ITEM(Task) %10TAGS(Context) %5Effort(Effort){:} %5CLOCKSUM(Clock)")
              
              (setq org-drawers (quote ("PROPERTIES" "LOGBOOK" "CLOCK")))
              (setq org-log-done (quote time))
              (setq org-log-into-drawer t)

              ;; org-capture
              (defvar hp-org-capture-templates
                '(("t" "TODOをInboxに追加する" entry (file+headline org-default-notes-file "Inbox") "** TODO %?\n   :PROPERTIES:\n   :CREATE: %U\n   :END:")
                  ("r" "興味のある本を追加する" entry (file+headline "~/org/book.org" "Inbox") "** TODO %?\n\t")
                  ("i" "Add interrupted task" entry (file+headline "~/src/org/diary.org" "Inbox") "** %?\n\t" :clock-in t :clock-resume t)         
                  ("w" "英単語をEnglish > 英単語に追加する" checkitem (file+olp org-default-notes-file "English" "英単語") "- [ ] %?\n\t"))
                "org-captureテンプレート")
              (setq org-capture-templates hp-org-capture-templates)
              
              ;; Save clock data in the CLOCK drawer and state changes and notes in the LOGBOOK drawer
              (setq org-clock-into-drawer "CLOCK")
              ))
            ;;(abbrev-mode 1)
            ;;(electric-pair-mode t)
            ;;(electric-indent-mode t)
            ;;(electric-layout-mode t)
  )
  )

;; ;; org-babel
;; (org-babel-do-load-languages 'org-babel-load-languages
;;                              '((emacs-lisp . t)
;;                                (perl . t)
;;                                (python . t)
;;                                (js . t)))

;; 		        ;; org-habit
;; 		        ;; (autoload 'org-habit-mode "org-habit" nil t)
;; 		        ;; (when (require 'org-habit nil t))
                
;;                 ;; Libre Office Writer
;;                 (setq org-export-odt-convert-processes 
;;                       '(("LibreOffice" "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i")
;;                         ("unoconv" "unoconv -f %f -o %d %i")))))
 
(defun hp-load-org-agenda-mode-config ()
  "org-agenda-modeに関する設定を読み込む. プライベートな関数として扱うこと."

  (when (require 'org-agenda nil t)
    (defun hp-load-org-agenda-custom-commands ()
      "Private function."
      (org-add-agenda-custom-command
       '("c" "2週間分の予定を表示"
         ((agenda "" ((org-agenda-span 14)
                      (org-agenda-time-grid nil)
                      (org-scheduled-past-days 10)
                      (org-deadline-warning-days 10)
                      (org-agenda-repeating-timestamp-show-all nil)
                      (org-agenda-skip-function 
                       '(org-agenda-skip-entry-if 'todo 'done)))))))
      
      ;; TODOステータスが"WAITING"であるタスクを列挙する
      (org-add-agenda-custom-command
     '("W" "Waiting for a response task list" 
       ((todo "WAITING"))))
      
      ;; タグ"OFFICE"で，かつTODOステータスがTODOもしくはWAITINGのタスクを列挙する
      (org-add-agenda-custom-command
       '("O" "@Office (TODO & WAITING only)" 
         ((tags "+@OFFICE/!+TODO|+WAITING"))))
      
      ;; タグ"HOME"で，かつTODOステータスがTODOもしくはWAITINGのタスクを列挙する
      (org-add-agenda-custom-command
       '("H" "@HOME (TODO & WAITING only)" 
         ((tags "+@HOME/!+TODO|+WAITING"))))
      
      ;; Z
      (org-add-agenda-custom-command
       '("Z" "Weekly review" 
         ((tags "+WEEKLY_REVIEW=\"t\""))))
      
      ;; タグ"READING"で，かつTODOステータスがTODOもしくはWAITINGのタスクを列挙する
      (org-add-agenda-custom-command
       '("R" "Reading task (TODO & WAITING only)" 
         ((tags "+READING/!+TODO|+WAITING"))))
      
      (org-add-agenda-custom-command
       '("N" "スケジュール未定のタスク" 
         ((todo "TODO"
                ((org-agenda-overriding-header "No due date")
                 (org-agenda-skip-function
                '(org-agenda-skip-entry-if 'scheduled 'deadline)))))))
      
      (org-add-agenda-custom-command
       '("D" "デッドライン付きタスクを表示" 
         ((agenda "" ((org-agenda-time-grid nil)
                      (org-deadline-warning-days 365) 
                      (org-agenda-entry-types '(:deadline)))))))
      
      (org-add-agenda-custom-command
       '("B" "Review today"
         ((agenda "" 
                  ((tags "OFFICE")
                   (org-agenda-span 'day)
                   (org-deadline-warning-days 3)
                   (org-agenda-sorting-strategy '(time-up todo-state-up priority-down))))
          (todo "WAITING"))))
      
      (org-add-agenda-custom-command
       '("A" "Review this week"
         ((agenda "" 
                  ((org-agenda-time-grid nil)
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-sorting-strategy '(time-up todo-state-up priority-down))
                   (org-scheduled-past-days 14)
                   (org-deadline-warning-days 14)))
          (todo "WAITING")
          (tags-todo "Payment"
                     ((org-agenda-overriding-header "Payment tasks")
                      (org-agenda-sorting-strategy
                       '(todo-state-down effort-up category-keep))))
          (tags-todo "Scheduling"
                     ((org-agenda-overriding-header "Scheduling")
                      (org-agenda-sorting-strategy
                       '(todo-state-down effort-up category-keep))))
          (tags-todo "Writting"
                     ((org-agenda-overriding-header "Writting")
                      (org-agenda-sorting-strategy
                       '(todo-state-down effort-up category-keep))))    
          (tags-todo "@OFFICE")
          (tags-todo "@HOME")
          (tags-todo "READING"))))
      
      (org-add-agenda-custom-command
       '("h" "Habits" 
         ((tags-todo "STYLE=\"habit\""
                     ((org-agenda-overriding-header "Habits")
                    (org-agenda-sorting-strategy
                     '(todo-state-down effort-up category-keep))
                    ))))))
    (add-hook 'org-agenda-mode-hook
              (lambda ()
                (setq org-agenda-files (list org-directory)) ;; アジェンダに表示する対象のファイル
                (hl-line-mode 1) ;; アジェンダ表示時にカーソル行をハイライトする
                (setq org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 7 :fileskip0 t :compact t :narrow 80))) ;; アジェンダでのclock reportの設定
                (hp-load-org-agenda-custom-commands) ;; org-agenda-custom-commandの設定を読み込む.
                ) 
              ) 
    ))

(defun hp-load-auto-install-config ()
  "auto-installに関する設定をする. プライベートな関数として扱うこと."  
  (when (require 'auto-install nil t)
    (setq auto-install-directory "~/.emacs.d/site-lisp/auto-install/")
    
    ;; 開発者曰く，互換性確保のために必要とのこと
    ;; http://d.hatena.ne.jp/rubikitch/20091221/autoinstall
    (auto-install-compatibility-setup)))

(defun hp-emacs-lisp-mode-hook ()
  "emacs-lisp-modeに関する設定をする。プライベートな関数として扱うこと。"
  (message "Load hp-emacs-lisp-mode-hook")
  (hs-minor-mode 1))

(defun hp-load-cmake-mode-config ()
  "cmake-modeに関する設定をする. プライベートな関数として扱うこと."  
  (when (locate-library "cmake-mode")
    (autoload 'cmake-mode' "cmake-mode" "" t)
    (add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
    (add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))))

;; 調子が悪いのでメンテナンスが必要.
;; (defun hp-load-flycheck-config ()
;;   "flycheckに関する設定をする. プライベートな関数として扱うこと."  
;;   (when (require 'flycheck nil t)
;;     (add-hook 'after-init-hook #'global-flycheck-mode)
;;     ;;(require 'helm-flycheck)
;;     (eval-after-load 'flycheck
;;       '(define-key flycheck-mode-map (kbd "C-+") 'helm-flycheck))
;;     (add-to-list 'flycheck-checkers 'swift)))

;; ;; ;; flycheck
;; ;; (autoload 'flycheck-mode "flycheck" nil t)
;; ;; (with-eval-after-load 'flycheck-mode
;; ;;   (define-key global-map (kbd "C-x C-n") 'flycheck-next-error)
;; ;;   (define-key global-map (kbd "C-x C-p") 'flycheck-previous-error)
;; ;;   (define-key global-map (kbd "C-x C-l") 'list-flycheck-errors))

;; (defun hp-load-yas-config ()
;;   "yasに関する設定をする. プライベートな関数として扱うこと."  
;;   (autoload 'yas-global-mode "yasnippet" nil t)
;;   (with-eval-after-load 'yas-global-mode
;;     ;; ユーザ定義のスニペットを保存するフォルダ
;;     (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
    
;;     ;; 操作とキーボード
;;     (custom-set-variables '(yas-trigger-key "TAB"))
    
;;     ;; 既存スニペットを挿入する
;;     (define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
    
;;     ;; 新規スニペットを作成するバッファを用意する 
;;     (define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet) 
    
;;     ;; 既存スニペットを閲覧・編集する
;;     (define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)))

(defun hp-load-ruby-mode-config ()
  "ruby-modeに関する設定をする. プライベートな関数として扱うこと."
  ;; ruby-mode
  (autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
  (with-eval-after-load 'ruby-mode
    (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
    (add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
    (add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
    (add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode)))
)

(defun hp-load-js2-mode-config ()
  "js2-modeに関する設定をする. プライベートな関数として扱うこと."
  ;; js2-mode
  (autoload 'js2-mode "js2-mode" nil t)
  (with-eval-after-load 'js2-mode
    (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
    (add-hook 'js2-mode-hook
              '(lambda ()
                 (setq js2-bounce-indent-flag nil)
                 (define-key js2-mode-map "\C-m" 'newline-and-indent)
                 (define-key js2-mode-map "\C-i" 'move-cursor-if-needed-when-indentation)
                 (company-mode t)
                 (tern-mode t))))
  
  (defun move-cursor-if-needed-when-indentation ()
    (interactive)
    (indent-for-tab-command)
    (let ((point-of-indentation
           (save-excursion
             (back-to-indentation)
             (point))))
      (skip-chars-forward "\s " point-of-indentation))))

(defun hp-load-web-mode-config ()
  "web-modeに関する設定をする. プライベートな関数として扱うこと."
  (autoload 'web-mode "web-mode" nil t)
  (with-eval-after-load 'web-mode
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))))

(defun hp-load-company-mode-config ()
    "company-modeに関する設定をする. プライベートな関数として扱うこと."
    (autoload 'company-mode "company" nil t)
    (with-eval-after-load 'company-mode      
      ;; 全バッファで有効にする 
      (global-company-mode) 
      
      ;; デフォルトは0.5
      (setq company-idle-delay 0)
      
      ;; デフォルトは4
      (setq company-minimum-prefix-length 2)
      
      ;; 候補の一番下でさらに下に行こうとすると一番上に戻る  
      (setq company-selection-wrap-around t)
      
      (when (require 'irony nil t)
        (add-hook 'c-mode-hook 'irony-mode)
        (add-hook 'c++-mode-hook 'irony-mode)
        (add-hook 'objc-mode-hook 'irony-mode)
        (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
        (add-to-list 'company-backends 'company-irony))))

;; org-time-stamp, org-time-stamp-inactiveの曜日表記を英語にする
(setq system-time-locale "C")

;; 端末でEmacsを開いた時の処理
(when (equal window-system nil)
  ;; emacsclientを使えるようにserver-startを実行する
  (when (not (server-running-p))
    (server-start))

  ;; macOSのクリップボードとEmacsのクリップボードを同期する    
  (defun hp-copy-from-macOS ()
    "macOSのクリップボードからペースト"
    (shell-command-to-string "pbpaste"))
  (defun hp-paste-to-macOS (text &optional push)
    "macOSのクリップボードにコピー"
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  (setq interprogram-cut-function 'hp-paste-to-macOS)
  (setq interprogram-paste-function 'hp-copy-from-macOS))

(when (require 'ivy nil t)

  ;; M-o を ivy-hydra-read-action に割り当てる．
  (when (require 'ivy-hydra nil t)
    (setq ivy-read-action-function #'ivy-hydra-read-action))

  ;; `ivy-switch-buffer' (C-x b) のリストに recent files と bookmark を含める．
  (setq ivy-use-virtual-buffers t)

  (setq enable-recursive-minibuffers t)

  ;; ミニバッファでコマンド発行を認める
  (when (setq enable-recursive-minibuffers t)
    (minibuffer-depth-indicate-mode 1)) ;; 何回層入ったかプロンプトに表示．

  ;; ESC連打でミニバッファを閉じる
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)

  ;; プロンプトの表示が長い時に折り返す（選択候補も折り返される）
  (setq ivy-truncate-lines nil)

  ;; リスト先頭で `C-p' するとき，リストの最後に移動する
  (setq ivy-wrap t)

  ;; アクティベート
  (ivy-mode 1))

(when (require 'counsel nil t)

  ;; キーバインドは一例です．好みに変えましょう．
  ;;(global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-s") 'swiper-isearch)
  ;;(global-set-key (kbd "C-e") 'counsel-imenu)  
  ;;(global-set-key (kbd "M-x") 'counsel-M-x)
  ;;(global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)

  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  ;;(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

  (global-set-key (kbd "C-c k") 'counsel-rg)
  ;; (global-set-key (kbd "C-M-z") 'counsel-fzf)
  
  ;; (global-set-key (kbd "M-y") 'counsel-yank-pop)

  (global-set-key (kbd "C-r") 'counsel-recentf)
  ;; (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)

  ;; アクティベート
  (counsel-mode 1))

(when (require 'ivy-posframe nil t)
  (setq ivy-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)
          (internal-border-width . 2)
          ;; (font . "DejaVu Sans Mono-10.75:hintstyle=hintfull")
          ))
  (setq ivy-posframe-border-width 1)
  
  (setq ivy-posframe-height-alist
        '((swiper . 15)
          (swiper-isearch . 15)
          (t . 10)))
  (setq ivy-posframe-display-functions-alist
        '((complete-symbol . ivy-posframe-display-at-point)
          (swiper . nil)
          (swiper-isearch . nil)
          (t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode 1))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (php-mode leaf-convert leaf-tree leaf leaf-keywords exec-path-from-shell counsel helm-ag zoom-window yasnippet web-mode use-package undohist undo-tree swift-mode ruby-additional plantuml-mode osx-dictionary neotree markdown-mode js2-mode helm-swoop helm-gtags foreign-regexp flycheck dired-sidebar dired-recent company-irony cmake-mode)))
 '(php-manual-url nil t)
 '(php-mode-coding-style nil t)
 '(php-mode-template-compatibility nil t)
 '(php-project-auto-detect-etags-file t t))

(message "Loaded init.el")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background nil :foreground "gray0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "MigMix 2M"))))
 '(bold ((t (:bold t))))
 '(bold-italic ((t (:bold t :italic t))))
 '(dired-directory ((t (:foreground "Blue1" :bold nil))))
 '(dired-header ((t (:foreground nil :background nil :bold t))))
 '(dired-mark ((t (:foreground "dark cyan" :bold t))))
 '(dired-marked ((t (:inherit dired-mark))))
 '(dired-symlink ((t (:foreground "Purple" :bold nil))))
 '(helm-bookmark-directory ((t (:inherit helm-ff-directory))))
 '(helm-bookmark-file ((t (:inherit helm-ff-file))))
 '(helm-buffer-directory ((t (:inherit helm-ff-directory))))
 '(helm-buffer-file ((t (:inherit helm-ff-file))))
 '(helm-etags-file ((t (:inherit helm-ff-file))))
 '(helm-ff-directory ((t (:inherit dired-directory))))
 '(helm-ff-dotted-directory ((t (:inherit helm-ff-directory))))
 '(helm-ff-dotted-symlink-directory ((t (:inherit helm-ff-symlink))))
 '(helm-ff-executable ((t (:inherit helm-ff-file))))
 '(helm-ff-file ((t (:inherit default))))
 '(helm-ff-invalid-symlink ((t (:inherit error))))
 '(helm-ff-symlink ((t (:inherit dired-symlink))))
 '(helm-ff-truename ((t (:inherit helm-ff-symlink))))
 '(helm-grep-file ((t (:inherit helm-ff-file))))
 '(helm-selection ((t (:inherit highlight))))
 '(helm-selection-line ((t (:inherit highlight))))
 '(helm-source-header ((t (:foreground nil :background "#f1f1f1" :bold t))))
 '(helm-visible-mark ((t (:inherit highlight))))
 '(highlight ((t (:foreground "gray0" :background "#D6EAF8" :bold t))))
 '(isearch ((t (:foreground nil :background "Yellow" :bold t))))
 '(italic ((t (:italic t))))
 '(lazy-highlight ((t (:foreground nil :background "light yellow" :bold nil))))
 '(minibuffer-prompt ((t (:foreground "#0062A0" :bold t))))
 '(mode-line ((t (:foreground "#ffffff" :background "#0062A0"))))
 '(mode-line-inactive ((t (:foreground "#000000" :background "#f1f1f1"))))
 '(region ((t (:background "#D6EAF8"))))
 '(show-paren-match ((t (:background "Yellow"))))
 '(underline ((t (:underline t)))))
