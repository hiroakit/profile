;; LICENSE
;;
;; ruby mode configration
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

(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))

;; ruby-electric (ELPAからダウンロードしたものが調子よくない模様)
;; (when (require 'ruby-electric nil t)
;;   (add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
;;   (setq ruby-electric-expand-delimiters-list nil)
;; )

;; ruby-block.el
(when (require 'ruby-block nil t)
  (ruby-block-mode t)
  (setq ruby-block-highlight-toggle t)
)
