;; LICENSE
;;
;; C# mode configration
;; Copyright (C) 2013-2015 Hiroaki ENDOH
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

(when (locate-library "csharp-mode")
  (autoload 'csharp-mode' "csharp-mode" "" t)
)

(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

;; カスタマイズ
(defun hp-csharp-mode-hook ()
  (electric-pair-mode 1)
  (auto-complete-mode t)
)

;; カスタマイズ割当
(add-hook 'csharp-mode-hook 'hp-csharp-mode-hook)
