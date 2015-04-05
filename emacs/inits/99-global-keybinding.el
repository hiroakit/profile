;; LICENSE
;;
;; global map configration
;; Copyright (C) 2014 Hiroaki ENDOH
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

;; helm-mode
(when (require 'helm-config nil t)
  ;; Emacsのコマンドを絞り込むためのキーバインディグ
  (define-key global-map (kbd "M-x")     'helm-M-x)

  ;; ファイル探す際に絞り込むためのキーバインディグ
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)

  ;; 最近開いたファイルを絞り込むためのキーバインディグ
  (define-key global-map (kbd "C-x C-r") 'helm-recentf)

  ;; キリングを絞り込むためのキーバインディング
  (define-key global-map (kbd "M-y")     'helm-show-kill-ring)

  ;; バッファ内に存在する関数を絞り込むためのキーバインディグ
  (define-key global-map (kbd "C-c i")   'helm-imenu)

  ;; バッファを絞り込むためのキーバインディグ
  (define-key global-map (kbd "C-x C-b") 'helm-buffers-list)

  ;; 絞り込み対象をとても広く取ってから絞り込むためのキーバインディグ
  (define-key global-map (kbd "C-x C-x") 'helm-for-files)

  ;; 絞り込み対象をやや広く取ってから絞り込むためのキーバインディグ
  (define-key global-map (kbd "C-x C-m") 'helm-mini)

  ;; 前回のhelmコマンドの続きから絞り込むためのキーバインディグ
  (define-key global-map (kbd "M-r")     'helm-resume))

;; org-mode (org-modeは一番優先する)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-co" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
