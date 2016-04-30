;; LICENSE
;;
;; My configration for Emacs
;; Copyright (C) 2013-2014 Hiroaki ENDOH
;;
;; This program is free software; you can redistribute it and/or modify 
;; it under the terms of the GNU General Public License as published 
;; by the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, 
;; but WITHOUT ANY WARRANTY; without even the implied warranty of 
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;; See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License 
;; along with this program. 
;; If not, see <http://www.gnu.org/licenses/>.

(defvar HEDefaultFrameWidthSize 120 "フレームの横幅の初期値を返します")
(defvar HEDefaultTabSpaceSize 4 "タブを半角スペースで置き換える際の文字数を返します")

;; 起動時のスプラッシュイメージを表示しない
(setq inhibit-startup-screen t)

;; スクロールバー非表示
(set-scroll-bar-mode nil)

;; タイトルバーにファイルのフルパス表示
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; 括弧の範囲内を強調表示
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

;; 括弧の範囲色
(set-face-background 'show-paren-match-face "#500")

;; リージョンの色
(set-face-background 'region "#4a4a4a")

;; モードラインに行番号表示
(line-number-mode t)

;; モードラインに列番号表示
(column-number-mode t)

;; ドラッグ&ドロップ, あるいは右クリック経由のコンテキストメニューなどで
;; ファイルを開くときには新しいウィンドウで開かずに新規バッファで開く
(define-key global-map [ns-drag-file] 'ns-find-file)
(setq ns-pop-up-frames nil)

;; buffer-nameを識別しやすくする設定
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; バッファを起動時に読み込む
(global-auto-revert-mode 1)

;; スクロールを一行ずつにする
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)

;; 1行80文字を表示するようフレームを調整する
(modify-frame-parameters nil (list (cons 'width HEDefaultFrameWidthSize)))

;; symlinkを追いかけるかの問いに, 常にYESと返す
(setq vc-follow-symlinks t)

;; 対応する括弧を光らせる
(show-paren-mode t)

;; 大文字と小文字を区別しない
(setq completion-ignore-case t)

;; タブで字下げする場合に半角スペースを利用する
(setq-default indent-tabs-mode nil)
(setq-default tab-width HEDefaultTabSpaceSize)

;; 長い文字列は右端で折り返す
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

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

;; 矩形選択
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; 日付取得
(defun hp-year () 
  (interactive) 
  (insert (format-time-string "%Y"))) 
(defun hp-month () 
  (interactive) 
  (insert (format-time-string "%m"))) 
(defun hp-day () 
  (interactive) 
  (insert (format-time-string "%d"))) 

(global-set-key (kbd "C-c C-d y") 'hp-year)
(global-set-key (kbd "C-c C-d m") 'hp-month)
(global-set-key (kbd "C-c C-d d") 'hp-day)

;;; 各パッケージのパスをload-pathに展開する
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

(defun hp-show-core-conf ()
  "init.elを開きます"
  (interactive)
  (switch-to-buffer (find-file-noselect hp-core-conf)))

(defun hp-show-org-conf ()
  "org-modeの設定ファイルを開きます"
  (interactive)
  (switch-to-buffer (find-file-noselect hp-org-conf)))

