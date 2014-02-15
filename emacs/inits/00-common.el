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

(defvar HEDefaultFrameWidthSize 80 "フレームの横幅の初期値を返します")
(defvar HEDefaultTabSpaceSize 4 "タブを半角スペースで置き換える際の文字数を返します")

;; 起動時のスプラッシュイメージを表示しない
(setq inhibit-startup-screen t)

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
