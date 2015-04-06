;;; フォント設定

(defvar HPDefaultFontSize 14 "フォントサイズの初期値を返します")

(defun my-ja-font-setter (spec)
  (set-fontset-font nil 'japanese-jisx0208 spec)
  (set-fontset-font nil 'katakana-jisx0201 spec)
  (set-fontset-font nil 'japanese-jisx0212 spec)
  (set-fontset-font nil '(#x0080 . #x024F) spec)
  (set-fontset-font nil '(#x0370 . #x03FF) spec)
  (set-fontset-font nil 'mule-unicode-0100-24ff spec))
 
(defun my-ascii-font-setter (spec)
  (set-fontset-font nil 'ascii spec))      
 
(cond
 ;; CocoaEmacs
 ((eq window-system 'ns)
  (when (or (= emacs-major-version 23) (= emacs-major-version 24))
    (let
          ;; 1) Monaco, Hiragino/Migu 2M : font-size=12, -apple-hiragino=1.2
          ;; 2) Inconsolata, Migu 2M     : font-size=14, 
          ;; 3) Inconsolata, Hiragino    : font-size=14, -apple-hiragino=1.0
          ((font-size HPDefaultFontSize)
           (ascii-font "MigMix 2M")
           (ja-font "MigMix 2M"))
        (my-ascii-font-setter (font-spec :family ascii-font :size font-size))
        (my-ja-font-setter (font-spec :family ja-font :size font-size)))
 
      ;; Fix ratio provided by set-face-attribute for fonts display
      (setq face-font-rescale-alist
            '(("^-apple-hiragino.*" . 1.0) ; 1.2
              (".*Migu.*" . 1.2)
              (".*Inconsolata.*" 1.0)
              (".*osaka-bold.*" . 1.0)     ; 1.2
              (".*osaka-medium.*" . 1.0)   ; 1.0
              (".*courier-bold-.*-mac-roman" . 1.0) ; 0.9
              ("-cdac$" . 1.0)))           ; 1.3
      ;; Space between lines
      (set-default 'line-spacing 1)
      ;; Anti aliasing with Quartz 2D
      (setq mac-allow-anti-aliasing t)))
 ((eq window-system 'w32)
  ;; (let
  ;;     (
  ;;      (font-size 24)
  ;;      (ascii-font "MigMix 1M")
  ;;      (ja-font "MigMix 1M")
  ;;     )
  ;;   (my-ascii-font-setter (font-spec :family ascii-font :size font-size))
  ;;   (my-ja-font-setter (font-spec :family ja-font :size font-size)))

  ;; default        : デフォルトフォント
  ;; variable-pitch : プロポーショナルフォント
  ;; fixed-pitch    : 等幅フォント
  ;; tooltip        : ツールチップ用フォント
  (set-face-attribute 'default nil :family "Migu 1M" :height 100)
  (set-face-attribute 'variable-pitch nil :family "Migu 1M" :height 100)
  (set-face-attribute 'fixed-pitch nil :family "Migu 1M" :height 100)
  (set-face-attribute 'tooltip nil :family "Migu 1M" :height 90)

  ;; Fix ratio provided by set-face-attribute for fonts display
  (setq face-font-rescale-alist '((".*Migu.*" . 1.0)))

  ;; Space between lines
  (set-default 'line-spacing 1))
)
