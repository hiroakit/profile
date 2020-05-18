;;; init.el --- My init.el  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Hiroaki ENDOH
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Initialization file for Emacs
;; You should compile using the following command:
;; Example: $emacs --batch -f batch-byte-compile init.el
;;

;;; Code:
;;
;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/{{pkg}}/init.el
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))


;;------------------------------------
;; Install leaf.el & configure
;;------------------------------------

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "http://orgmode.org/elpa/")
                       ("melpa" . "http://melpa.org/packages/")
                       ("gnu"   . "http://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)
    :config
    (leaf-keywords-init)))

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)

  ;; 
  ;; M-x leaf-tree-mode
  ;;
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

;;-------------------------
;; Base configuration
;;-------------------------

(leaf doom-themes
  :doc "See https://github.com/hlissner/emacs-doom-themes"
  :ensure t
  :custom
  (doom-themes-enable-italic . nil)
  (doom-themes-enable-bold . nil)
  :config
  (load-theme 'doom-one-light t)
  (doom-themes-org-config))

;; Startup Emacs
(leaf startup
  :config
  ;; (set-face-background 'region "LightSkyBlue")
  ;; (set-face-foreground 'region "Black")
  ;; (set-face-background 'vertical-border "DarkGray")
  ;; (set-face-foreground 'vertical-border (face-background 'vertical-border))
  ;; (setq default-directory "~/")
  ;; (setq command-line-default-directory "~/")
  (setq eol-mnemonic-dos "(CRLF)")
  (setq eol-mnemonic-mac "(CR)")
  (setq eol-mnemonic-unix "(LF)")
  :preface
  (defun hiroakit/text-scale-up ()
    (interactive)
	(text-scale-increase 1))
  (defun hiroakit/text-scale-down ()
    (interactive)
	(text-scale-decrease 1))
  (defun hiroakit/text-scale-reset ()
    (interactive)
	(text-scale-set 0))
  :bind
  (("C-<wheel-up>" . hiroakit/text-scale-up)
   ("C-<wheel-down>" . hiroakit/text-scale-down)
   ("M-0" . hiroakit/text-scale-reset)
   ("C-j" . goto-line)
   ("C-c t" . toggle-truncate-lines)
   ("C-c r" . rename-file))
  :config
  ;; (set-frame-parameter nil 'alpha 80)
  :custom
  ((inhibit-startup-screen            . t)
   (inhibit-startup-message           . t)
   (inhibit-startup-echo-area-message . t)
   (initial-scratch-message           . nil)))

(leaf cus-edit
  :doc "Customizing Emacs Lisp packages"
  :tag "builtin" "faces" "help"
  :custom
  `((tool-bar-mode . nil)
    (scroll-bar-mode . nil)
    (menu-bar-mode . nil)
    (blink-cursor-mode . nil)
    (column-number-mode . nil)
    (ns-transparent-titlebar . t)
    (vertical-scroll-bars . nil)
    (internal-border-width . 0)
    (ring-bell-function . 'ignore)
    (tab-width . 4)
    (indent-tabs-mode . nil)
    (truncate-lines . t)
    (truncate-partial-width-windows . t)
    (custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf paren
  :doc "Highlighting the corresponding brackets"
  :custom
  ((show-paren-style  . 'mixed))
  :hook
  (emacs-startup-hook . show-paren-mode))

(leaf hl-line
  :doc "Highlighting current line"
  :hook
  (emacs-startup-hook . global-hl-line-mode))

(leaf undo-tree
  :doc "Undo / Redo"
  :ensure t
  :init
  (global-undo-tree-mode)
  :bind
  (("C-z" . undo)
   ("C-S-z" . redo)
   (:undo-tree-visualizer-mode-map
    :package undo-tree
    ("C-g" . undo-tree-visualizer-quit)
    ;; ("ESC" . undo-tree-visualizer-quit) ;; How to bind to ESC?
    ("RET" . undo-tree-visualizer-quit))))

(leaf flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(leaf neotree
  :doc "Filer"
  :ensure t
  :bind
  (("C-q" . neotree-toggle)
   (:neotree-mode-map
	("RET" . neotree-enter)))
  :custom
  ((neo-smart-open . t)
   (neo-create-file-auto-open . t))
  ;;(neo-theme . (if (display-graphic-p) 'icons 'arrow))
  )

(leaf helm
  :doc "Filter information"
  :ensure t
  :init
  (require 'helm-config)
  (helm-mode t)
  (custom-set-faces
   ;; Setting typeface colors
   '(helm-ff-directory ((t (:inherit dired-directory))))
   '(helm-ff-dotted-directory ((t (:inherit dired-directory))))
   '(helm-ff-invalid-symlink ((t (:foreground "DarkGray"))))
   '(helm-buffer-directory ((t (:inherit dired-directory))))
   '(helm-match ((t (:foreground "Black" :weight bold)))))
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x C-x" . helm-buffers-list)
   ("C-x C-r" . helm-recentf))
  :config
  (leaf helm-swoop
    :load-path `,(locate-user-emacs-file "site-lisp/helm-swoop")
    :custom (helm-swoop-pre-input-function
             . (lambda ()
                 (if mark-active
                     (buffer-substring-no-properties (mark) (point))
                   "")))
    :bind
    (("C-S-s" . helm-swoop)
     ("C-c f" . hydra-helm-swoop/body)
     (:helm-swoop-map
      ("C-s" . helm-multi-swoop-all-from-helm-swoop)))))

(leaf helm-projectile
  :ensure t
  :bind
  (("C-c p f" . helm-projectile-find-file)
   ("C-c p e" . helm-projectile-recentf)
   ("C-c p g" . helm-projectile-grep)
   ("C-c p p" . helm-projectile-switch-project))
  ;; :init
  ;; (leaf helm-ag)
  ;; :config
  ;; (projectile-global-mode t)
  ;; (helm-projectile-on))
)

;;-------------------------
;; org series
;;-------------------------

(leaf org-mode
  :mode
  ("\\.org\\'")
  ;;  :config
  ;;  ((setq org-directory "~/Documents/org")
  ;;   (setq org-default-notes-file (concat (file-name-as-directory org-directory) "inbox.org")))
  :custom
  `((org-startup-folded . t)
   (org-startup-truncated . t)
   (org-return-follows-link . t)
   (org-todo-keywords . '((sequence "TODO(t)" "|" "DONE(d!)") (sequence "WAITING(w@/!)" "|") (sequence "|" "CANCELED(c@/!)")))
   (org-tag-alist . '(("@HOME" . ?h)
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
   ))

(leaf org-agenda
  :bind
  (("C-c a" . org-agenda))
  :custom
  (org-agenda-span . 'day)
  (org-agenda-format-date . "%Y/%m/%d (%a)")
  (org-agenda-start-on-weekday . 1)
  (org-agenda-custom-commands .
                              '(("c" "Get agenda & TODO list."
                                ((agenda "" ((org-agenda-ndays 1)
                                             (org-agenda-entry-types '(:timestamp :sexp))))
                                 (todo "TODO" ((org-agenda-prefix-format " %i %-22:c")))
                                 (todo "WAITING" ((org-agenda-prefix-format " %i %-22:c"))))
                                ("W" "Waiting for a response task list"
                                 ((todo "WAITING"))))))
  :init
  ;;((add-to-list 'org-agenda-files "inbox.org")))
  )

(leaf org-babel
  :custom
  ((org-src-fontify-natively . t)
   (org-src-tab-acts-natively . t)
   (org-src-preserve-indentation . t)
   (org-edit-src-content-indentation . 0)))
      
    
(leaf google-translate
  :ensure t
  :custom
  (google-translate-default-source-language . "en")
  (google-translate-default-target-language . "ja"))

;;-------------------------
;; Launguages Support
;;-------------------------

(leaf editorconfig
  :ensure t
  :init
  (editorconfig-mode t))

(leaf c++-mode
  :mode
  ("\\.h\\'"
   "\\.cpp\\'")
  :init
  (electric-pair-mode 1)
  :bind
  ((:c++-mode-map
    ("C-c C-o" . ff-find-other-file))))

;; (leaf makefile-mode)
;; (leaf shell-script-mode)

(leaf cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'"
         "\\.cmake\\'"))

(leaf web-mode
  :ensure t
  :mode
  ("\\.html?\\'"
   "\\.jsx\\'"
   "\\.vue\\'")
  :custom
  `(
    (web-mode-markup-indent-offset . 4)
    (web-mode-code-indent-offset . 4)
    (web-mode-css-indent-offset . 4)
    (js-indent-level . 4)
    (web-mode-enable-auto-pairing . t)
    (web-mode-enable-auto-expanding . t)
    (web-mode-enable-css-colorization . t)
    (indent-tabs-mode . t)
    (tab-width . 4)
    (web-mode-html-offset . 4)
    (web-mode-css-offset . 4)
    (web-mode-script-offset . 4)
    (web-mode-php-offset . 4)))

(leaf php-mode
  :ensure t)

(leaf json-mode
  :ensure t)

(leaf yaml-mode
  :ensure t
  :mode
  ("\\.yml$'"))

(leaf toml-mode
  :ensure t
  :mode
  ("\\.toml$'"))

(leaf glsl-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.shader$" . glsl-mode)))

(leaf company
  :ensure t
  :init
  (global-company-mode 1)
  :bind
  (("<C-tab>" . company-complete)
   (:company-active-map
	:package company
    ("<tab>" . company-complete-selection)
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous))
   (:company-search-map
	:package company
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)))
  :config
  ;;(company-idle-delay . 0.5)
)

;;------------------------------------
;; LSP - Language Server Protocol
;;------------------------------------

(leaf lsp-mode
  :ensure t
  :commands lsp
  :hook
  (c++-mode-hook . lsp)
  :config
  :custom
  ((lsp-print-io . t)
   (lsp-prefer-flymake . nil)
   (lsp-response-timeout . 5)
   (lsp-clients-clangd-executable . "/usr/local/opt/llvm/bin/clangd")))

(leaf lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook
  ((lsp-mode . lsp-ui-mode))
  :bind
  (:lsp-ui-mode-map
   ("C-c C-r" . lsp-ui-peek-find-references)
   ("C-c C-j" . lsp-ui-peek-find-definitions)
   ("C-c i"   . lsp-ui-peek-find-implementation)
   ("C-c m"   . lsp-ui-imenu)
   ("C-c s"   . lsp-ui-sideline-mode)
   ("C-c d"   . ladicle/toggle-lsp-ui-doc))
  :custom
  ((lsp-ui-doc-enable . t)
   (lsp-ui-doc-header . t)
   (lsp-ui-doc-position . 'top) ;; top, bottom, or at-point
   (lsp-ui-doc-max-width . 150)
   (lsp-ui-doc-max-height . 30)
   (lsp-ui-doc-use-childframe . t)
   (lsp-ui-doc-use-webkit . t)
   (lsp-ui-sideline-enable . nil)
   (lsp-ui-sideline-ignore-duplicate . t)
   (lsp-ui-sideline-show-symbol . t)
   (lsp-ui-sideline-show-hover . t)
   (lsp-ui-sideline-show-diagnostics . nil)
   (lsp-ui-sideline-show-code-actions . nil)
   (lsp-ui-imenu-enable . t)
   (lsp-ui-imenu-kind-position . 'top) ;; top, bottom, or at-point
   (lsp-ui-imenu-window-width . 60)
   ;; (lsp-ui-imenu--custom-mode-line-format . )
   (lsp-ui-peek-enable . t)
   (lsp-ui-peek-peek-height . 20)
   (lsp-ui-peek-list-width . 50)
   (lsp-ui-peek-fontify . 'on-demand) ;; never, on-demand, or always
   ))

(leaf company-lsp
  :ensure t
  :commands company-lsp
  :after
  (lsp-mode lsp-ui company)
  :custom
  ((company-lsp-async . t)
   (company-lsp-cache-candidates . nil)
   ;; (company-lsp-enable-recompletion . t)
   ;; (company-lsp-enable-snippet . t)
   )
  )

(provide 'init)

;;; init.el ends here
