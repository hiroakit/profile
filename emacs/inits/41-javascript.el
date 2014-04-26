;; LICENSE
;;
;; j2mode configration
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

;; ELPAを使っている場合は, autoloadを明示的に設定しなくてよい
;; (autoload 'js2-mode "js2-mode" 
;;   "Mode for editing javascript source files" t)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
          '(lambda ()
             (setq js2-bounce-indent-flag nil)
             (define-key js2-mode-map "\C-m" 
               'newline-and-indent)
             (define-key js2-mode-map "\C-i" 
               'move-cursor-if-needed-when-indentation)
             (add-to-list 'ac-dictionary-files 
                          "~/.emacs.d/dict/ac-user-dict/apple-uiautomation")))
(defun move-cursor-if-needed-when-indentation ()
  (interactive)
  (indent-for-tab-command)
  (let ((point-of-indentation
         (save-excursion
           (back-to-indentation)
           (point))))
    (skip-chars-forward "\s " point-of-indentation)))
