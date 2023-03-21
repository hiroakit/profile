;;; init.el --- Initialize Emacs -*- lexical-binding: t; -*-
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
;;   emacs -q -l path/to/your/.emacs.d/init.el

(defconst user-default-font-name "Cica"
  "The font you want to use as the standard it in emacs")

;;------------------------------------
;; キープリフィックス
;;
;; https://www.emacswiki.org/emacs/PrefixKey
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Commands.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Keys.html#index-define_002dprefix_002dcommand
;;------------------------------------

;; M-z zap-to-charは手元の環境では使わないのでプリフィックスコマンドに変える
(define-prefix-command 'my-prefix-command)
(global-set-key (kbd "M-z") 'my-prefix-command)

;;------------------------------------
;; Install leaf.el & configure
;;------------------------------------

(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("melpa" . "http://melpa.org/packages/")
                       ("gnu"   . "http://elpa.gnu.org/packages/")
                       ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (package-initialize)

  (unless (package-installed-p 'exec-path-from-shell)
    (package-refresh-contents)
    (package-install 'exec-path-from-shell))

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

(set-language-environment "Japanese")
(setq vc-follow-symlinks t)

;;-------------------------
;; フレームサイズ調整
;;
;; https://m13o.net/202006052311
;;-------------------------

(defun adjust-pos-and-size-of-current-frame-to-center ()
  (interactive)
  (when (display-graphic-p)
    (let* ((workarea (mapcar (lambda (attr)
                               (cdddr (assoc 'workarea attr)))
                             (display-monitor-attributes-list)))
           (width (floor (* (seq-min (mapcar #'car workarea)) 0.6)))
           (height (floor (* (seq-min (mapcar #'cadr workarea)) 0.8))))
      (let* ((current-workarea (frame-monitor-workarea))
             (current-frame (selected-frame))
             (x 0)
             (y 0))
        (when (>= (nth 0 current-workarea) 0)
          (setq x (- (+ (nth 0 current-workarea) (/ (nth 2 current-workarea) 2)) (/ width 2))))
        (when (>= (nth 1 current-workarea) 0)
          (setq y (+ (/ (- (nth 3 current-workarea) height) 2) (nth 1 current-workarea))))
        (set-frame-position current-frame x y)
        (set-frame-size current-frame width height t)))))

;;-------------------------
;; 環境変数
;;
;; https://www.emacswiki.org/emacs/ExecPath
;; https://github.com/purcell/exec-path-from-shell
;;-------------------------

;; (dolist (x (split-string
;;             (shell-command-to-string "eval $(/usr/libexec/path_helper -s) && printf $PATH")
;;             path-separator))
;;   (print x))

(when (equal window-system 'ns)
  (exec-path-from-shell-initialize))

;;-------------------------
;; 警告音
;;-------------------------

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Beeping.html
;; https://www.emacswiki.org/emacs/AlarmBell
(when (equal system-type 'windows-nt)
  ;; 何故かWindows 11でのビープ音は気になってしまったので鳴らないようにした
  (setq ring-bell-function 'ignore)

  ;; サウンドファイルを指定できるらしい
  ;; (setq ring-bell-function (lambda ()
  ;;                            (play-sound-file "/path/to/sound.wav")))
  )

;;-------------------------
;; バッファ
;;-------------------------

;; このバッファは消えると不便に思えたから消せないようにする
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))

;; このバッファは消えると不便に思えたから消せないようにする
(with-current-buffer "*Messages*"
  (emacs-lock-mode 'kill))

;;-------------------------
;; フォント
;;
;; M-: (font-xlfd-name (font-at (point)))
;; M-x describe-char
;;-------------------------

(when (member user-default-font-name (font-family-list))
  ;; フォント設定と関数:
  ;;   set-frame-font
  ;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Frame-Font.html
  ;;
  ;;   set-fontset-font
  ;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Fontsets.html#index-set_002dfontset_002dfont
  ;;
  ;;   set-face-attribute
  ;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Attribute-Functions.html#index-set_002dface_002dattribute
  ;;
  ;;   set-face-foreground
  ;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Attribute-Functions.html#index-set_002dface_002dforeground
  ;;
  ;; 注意:
  ;;   org-modeのテーブルの縦棒にset-face-attribute 'defaultで指定したフォントが当たらないことがある
  ;;   手元の環境ではload-themeをearly-init.elで実行したときにその事象に遭遇した
  ;;   暫定対応でorg-mode-hookでフェイスorg-tableに対してset-face-attributeで再度フォントを指定している

  ;; シンプルに済ませたい場合はset-frame-fontのみでよい
  (set-frame-font (font-spec :family user-default-font-name :size 18))

  ;; set-face-attributeでdefaultに対してフォントを指定することが重要
  ;; ほかのことを調べたいので理由は追っていない
  ;; :heightでフォントサイズを変更できる
  ;; (set-face-attribute 'default nil :family user-default-font-name :height 240)

  ;; japanese-jisx0208はCharacter setsの１つ
  ;; M-x describe-launguage-environmentでjapaneseと入力するとCharacter setsを確認できる
  ;; font-specでsizeを指定しても反映されなかった
  ;; (set-fontset-font t 'japanese-jisx0208 (font-spec :family user-default-font-name))

  ;; 記号まわりで設定が必要になる...らしい
  ;; macOSでGUI付きEmacsを操作している際は特段困っていないためコメントアウトした
  ;; (setq use-default-font-for-symbols nil)
  )

;;-------------------------
;; テーマ
;;-------------------------

;; See early-init.el
;; (load-theme 'modus-vivendi-deuteranopia t)

(when (not (equal window-system nil))
  (load-theme 'modus-vivendi-tinted t))

;;-------------------------
;; 文字コード
;;-------------------------

(set-default 'buffer-file-coding-system 'utf-8)

;;-------------------------
;; 矩形選択
;;-------------------------

;; Emacs 24.4以降ではC-x SPCで矩形選択可
;; https://www.emacswiki.org/emacs/RectangleMark
;;
;; C-x SPCで矩形選択可能だがcua-modeはテキストの加筆できるなど機能が豊富
;; https://www.emacswiki.org/emacs/CuaMode
;;
;; cua-modeの矩形選択(cua-rectangle-mark-mode)をM-RETで開始する
;; これはターミナルでEmacsを動かした場合にC-RETだとReturnのCR (ASCII 13)と解釈され改行されるためで
;; M-RETを割り当て、GUI版EmacsとターミナルのEmacsでのキーバインドを統一した
;; https://www.fromkk.com/posts/c-m-ret-and-return-key-in-emacs/
(cua-mode t)
(global-set-key (kbd "M-RET") 'cua-rectangle-mark-mode)

;;-------------------------
;; Navigation
;;-------------------------

(leaf doom-modeline
  :ensure t
  :hook (after-init-hook . doom-modeline-mode)
  :custom `((doom-modeline-icon . nil)
            (doom-modeline-major-mode-icon . nil)
            (doom-modeline-major-mode-color-icon . nil)))

(leaf vertico
  :ensure t
  :custom (vertico-count . 10)
  :hook (after-init-hook . vertico-mode))

(leaf consult
  :ensure t
  :bind (("C-x C-x" . consult-buffer)
           ("C-x r b" . consult-bookmark)
           ("M-s f " . consult-find)
           ;; ("C-x C-g" . consult-grep)
           ("C-x C-r" . consult-recent-file)))

(leaf orderless
  :ensure t
  :custom `((completion-styles . '(orderless))))

(leaf recentf
  :hook (after-init-hook . recentf-mode))

;;-------------------------
;; UNDO/REDO
;;
;; https://www.emacswiki.org/emacs/UndoTree
;;-------------------------

(leaf undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :config (setq undo-tree-auto-save-history nil)
  :bind (("C-z" . undo)
         ("C-S-z" . redo)
         (:undo-tree-visualizer-mode-map
          :package undo-tree
          ("<escape>" . undo-tree-visualizer-quit)
          ("RET" . undo-tree-visualizer-quit))))

;;-------------------------
;; NEOTREE
;;-------------------------

(leaf neotree
  :doc "Filer"
  :ensure t
  :bind (("C-c n" . neotree-toggle)
         (:neotree-mode-map
          ("RET" . neotree-enter)))
  :custom  ((neo-smart-open . t)
            ;;(neo-theme . (if (display-graphic-p) 'icons 'arrow))
            (neo-create-file-auto-open . t)))

(defvar my/delayed-priority-low-configurations '())
(defmacro with-delayed-execution (&rest body)
  (declare (indent 0))
  `(setq my/delayed-priority-low-configurations
         (append my/delayed-priority-low-configurations ',body)))

;;-----------------------------------------------------
;; GREP
;;
;; deadgrep.el経由でripgrepを使う
;; ripgrepは ~/.ignore を参照して除外ファイルを認識する
;; ~/.ignoreに.git/を記載しておくと検索結果が見やすい (.gitディレクトリの中も検索したい場合は外すこと)
;;------------------------------------------------------

(leaf deadgrep
  :ensure t
  :bind ("C-x C-g" . deadgrep))  

;; deadgrepがripgrepに与える実行時引数を変える
(defun deadgrep--include-args (rg-args)
  ;; 隠しファイルも検索対象に含める
  (push "--hidden" rg-args)

  ;; シンボリックリンクは追跡する
  (push "--follow" rg-args))

(advice-add 'deadgrep--arguments :filter-return #'deadgrep--include-args)

;;-------------------------
;; Lisp
;;-------------------------

(leaf elisp-mode
  :bind ((emacs-lisp-mode-map
          ("C-M-b" . eval-buffer)
          ("C-M-r" . eval-region)))
  :custom `((indent-tabs-mode . nil)
            (tab-width . 4)))

;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (message "hoge1")

;;             ;; config
;;             (setq tab-width 2)
;;             (setq indent-tabs-mode nil)
;;             (setq show-trailing-whitespace t)
;;             (show-paren-mode t)
;;             (setq show-paren-style 'mixed)

;;             ;; keybind
;;             (define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)
;;             (define-key emacs-lisp-mode-map (kbd "C-c C-d") 'eval-region)
;;             (define-key emacs-lisp-mode-map (kbd "C-x C-e") 'pp-eval-last-sexp)
;;             (define-key emacs-lisp-mode-map (kbd "C-c C-r") 'reindent-then-newline-and-indent)))

;;-------------------------
;; C++
;;-------------------------

(leaf c++-mode
  :mode ("\\.h\\'"
         "\\.cpp\\'")
  :hook (c++-mode-hook . (lambda ()
                           (setq indent-tabs-mode nil)
                           (setq show-trailing-whitespace t)
                           (electric-pair-mode 1)))
  :bind ((:c++-mode-map
          ("C-c C-o" . ff-find-other-file))))

(leaf cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'"
         "\\.cmake\\'"))

;;-------------------------
;; C# & .NET
;;-------------------------

(leaf csharp-mode
  :ensure t
  :mode ("\\.cs[x]?\\'" . csharp-mode)
  :init
  (electric-pair-mode 1))

;; (leaf flycheck
;;   :ensure t
;;   :init
;;   (global-flycheck-mode t))

;;------------------------------------
;; Swift
;;------------------------------------

;; (leaf swift-mode :ensure t)

;;-------------------------
;; Launguages Support
;;-------------------------

;; (leaf editorconfig
;;   :ensure t
;;   :init
;;   (editorconfig-mode t))

;;-------------------------
;; org series
;;-------------------------

(leaf org
  :ensure t
  :mode ("\\.org\\'")
  :bind
  ((:org-mode-map ("M-z x" . org-cut-special)
                  ("M-z c" . org-copy-special)
                  ("M-z v" . org-paste-special)))
  
  :hook
  (org-mode-hook . (lambda ()
                     ;; org-modeのテーブルの縦棒にset-face-attribute 'defaultで指定したフォントが当たらないことがある
                     (set-face-attribute 'org-table nil :family user-default-font-name)

                     ;; org-modeでは補完機能を使わず文章を書きたい
                     (company-mode nil)

                     (visual-line-mode)

                     ;; (flyspell-mode)
                     )))

;; org-contribにob-csharp.elがある
(leaf org-contrib
  :ensure t)

(leaf org-babel
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (csharp . t)
     (python . t)))
  :custom
  ((org-src-fontify-natively . t)
   (org-src-tab-acts-natively . t)
   (org-src-preserve-indentation . t)
   (org-edit-src-content-indentation . 0)))

;; (leaf org-mode
;;   :mode
;;   ("\\.org\\'")
;;   :bind
;;   ("C-c c" . org-capture)
;;   (:org-mode-map
;;    ;; In ASCII, C-i and <TAB> are the same character.
;;    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Function-Keys.html
;;    ("C-c C-x C-i" . org-clock-in))
;;   :custom
;;   (org-directory . "~/Library/Mobile Documents/com~apple~CloudDocs/org")
;;   `(org-default-notes-file . ,(concat (file-name-as-directory org-directory) (file-name-nondirectory "capture.org")))
;;   ;;`(org-default-notes-file . ,(concat (file-name-as-directory "~/Documents/org") (file-name-nondirectory "capture.org")))
;;   `((org-startup-folded . t)
;;    (org-startup-truncated . t)
;;    (org-return-follows-link . t)
;;    (org-todo-keywords . '((sequence "TODO(t)" "|" "DONE(d!)") (sequence "WAITING(w@/!)" "|") (sequence "|" "CANCELED(c@/!)")))
;;    (org-tag-alist . '(("@HOME" . ?h)
;;                       ("@OFFICE" . ?o)
;;                       ("@TSUTAYA")
;;                       ("@ITOYOKADO" . ?i)
;;                       ("@CLEANERS" . ?x)
;;                       ("@POSTOFFICE" .?y)
;;                       ("@BANK" . ?b)
;;                       ("@LUNCHTIME")
;;                       ("PHONE" . ?p)
;;                       ("MAIL" . ?m)
;;                       ("READING" . ?r)
;;                       ("WATCHING")
;;                       ("CONFERENCE" . ?c)
;;                       ("TALKING" . ?t)
;;                       ("Scheduling" . ?s)
;;                       ("Writting" . ?w)
;;                       ("Payment")))))

;;   (leaf org-agenda
;;  :bind
;;  (("C-c a" . org-agenda))
;;  :custom
;;  (org-agenda-files . '("~/Library/Mobile Documents/com~apple~CloudDocs/org/notebook/private.org"))
;;  (org-agenda-span . 'day)
;;  (org-agenda-format-date . "%Y/%m/%d (%a)")
;;  (org-agenda-start-on-weekday . 1)
;;  (org-agenda-custom-commands .
;;                              '(("c" "Get agenda & TODO list."
;;                                    ((agenda "" ((org-agenda-ndays 1)
;;                                              (org-agenda-entry-types '(:timestamp :sexp))))
;;                                  (todo "TODO" ((org-agenda-prefix-format " %i %-22:c")))
;;                                  (todo "WAITING" ((org-agenda-prefix-format " %i %-22:c"))))
;;                                    ("W" "Waiting for a response task list"
;;                                  ((todo "WAITING"))))))
;;  :init
;;  ;;((add-to-list 'org-agenda-files "inbox.org")))
;;  )

;; (leaf org-capture
;;   :require
;;   org-capture
;;   :defvar
;;   org-capture-templates
;;   :config
;;   (add-to-list 'org-capture-templates
;;                `("a" "Add interrupted task"
;;                  entry (file+headline
;;                         ,(concat (concat (file-name-as-directory org-directory) (file-name-as-directory "work"))
;;                               (file-name-nondirectory "daily.org")) "Inbox")
;;                  "** TODO %? \n SCHEDULED: %^t \n"
;;                  :clock-in t
;;                  :clock-resume t))
;;   (add-to-list 'org-capture-templates
;;             '("i" "Add task"
;;               entry (file+headline "~/Documents/org/inbox.org" "Inbox")
;;               "** TODO %? \n SCHEDULED: %^t \n"
;;               :clock-in t
;;               :clock-resume t))
;;   (add-to-list 'org-capture-templates
;;             '("l" "Create new meeting log"
;;               entry (file+headline "~/Documents/org/work/daily.org" "Meeting")
;;                   "** TODO ^{title} %^g\n  %?\n  %a \n SCHEDULED: %^t \n"))
;;   (add-to-list 'org-capture-templates
;;             '("c" "Add cinema in list"
;;               entry (file+headline "~/Documents/org/capture.org" "Memo/Cinema")
;;               "** ^{title} %^g\n"))
;;   (add-to-list 'org-capture-templates
;;             '("b" "Add book in list"
;;               entry (file+headline "~/Documents/org/capture.org" "Memo/Books")
;;               "** ^{title} %^g\n"))
;;   (add-to-list 'org-capture-templates
;;             '("e" "Add item that Emacs customization improvements into the list"
;;               entry (file+headline "~/Documents/org/capture.org" "Memo/Emacs")
;;               "** ^{title} %^g\n"))
;;   (add-to-list 'org-capture-templates
;;             '("p" "Add tips in list"
;;               entry (file+headline "~/Documents/org/capture.org" "Memo/Tips")
;;               "** ^{title} %^g\n")))

;; (leaf ox
;;   :after org)

;;------------------------------------
;; Company - Completion Framework
;;------------------------------------

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
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.2)
)

;;------------------------------------
;; LSP - Language Server Protocol
;;------------------------------------

;; (leaf lsp-mode
;;   :ensure t
;;   :commands lsp
;;   :hook
;;   (c++-mode-hook . lsp)
;; ;;  (csharp-mode-hook . lsp-deferred)
;; ;;  (swift-mode-hook . lsp)
;;   :config
;;   :custom
;;   ((lsp-print-io . t)
;;    (lsp-prefer-flymake . nil)
;;    (lsp-prefer-capf . t)
;;    (lsp-response-timeout . 5)
;;    (lsp-clients-clangd-executable . "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clangd")))
;; ;;    (lsp-clients-clangd-executable . "/usr/local/opt/llvm/bin/clangd")))

(leaf eglot
  :ensure t
  :require t
  :config
;;  (add-to-list 'eglot-server-programs '(python-mode "pylsp"))
  (add-hook 'c++-mode-hook 'eglot-ensure))

;;(leaf python-mode
;;  :ensure t
;;  :hook
;;  (python-mode . eglot-ensure))

;; (leaf lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode
;;   :hook
;;   ((lsp-mode . lsp-ui-mode))
;;   :bind
;;   (:lsp-ui-mode-map
;;    ("C-c C-r" . lsp-ui-peek-find-references)
;;    ("C-c C-j" . lsp-ui-peek-find-definitions)
;;    ("C-c i"   . lsp-ui-peek-find-implementation)
;;    ("C-c m"   . lsp-ui-imenu)
;;    ("C-c s"   . lsp-ui-sideline-mode)
;;    ("C-c d"   . ladicle/toggle-lsp-ui-doc))
;;   :custom
;;   ((lsp-ui-doc-enable . t)
;;    (lsp-ui-doc-header . t)
;;    (lsp-ui-doc-position . 'top) ;; top, bottom, or at-point
;;    (lsp-ui-doc-max-width . 150)
;;    (lsp-ui-doc-max-height . 30)
;;    (lsp-ui-doc-use-childframe . t)
;;    (lsp-ui-doc-use-webkit . t)
;;    (lsp-ui-sideline-enable . nil)
;;    (lsp-ui-sideline-ignore-duplicate . t)
;;    (lsp-ui-sideline-show-symbol . t)
;;    (lsp-ui-sideline-show-hover . t)
;;    (lsp-ui-sideline-show-diagnostics . nil)
;;    (lsp-ui-sideline-show-code-actions . nil)
;;    (lsp-ui-imenu-enable . t)
;;    (lsp-ui-imenu-kind-position . 'top) ;; top, bottom, or at-point
;;    (lsp-ui-imenu-window-width . 60)
;;    ;; (lsp-ui-imenu--custom-mode-line-format . )
;;    (lsp-ui-peek-enable . t)
;;    (lsp-ui-peek-peek-height . 20)
;;    (lsp-ui-peek-list-width . 50)
;;    (lsp-ui-peek-fontify . 'on-demand) ;; never, on-demand, or always
;;    ))

;; (leaf lsp-sourcekit
;;   :ensure t
;;   :after lsp-mode
;;   :config (setq lsp-sourcekit-executableble "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

;;------------------------------------
;; Laungage translation
;;------------------------------------

;; (leaf google-translate
;;   :ensure t
;;   :custom
;;   (google-translate-default-source-language . "en")
;;   (google-translate-default-target-language . "ja"))

;;-------------------------
;; Other
;;-------------------------

;; (leaf makefile-mode)
;; (leaf shell-script-mode)

;; (leaf web-mode
;;   :ensure t
;;   :mode
;;   ("\\.html?\\'"
;;    "\\.jsx\\'"
;;    "\\.vue\\'")
;;   :custom
;;   `((web-mode-markup-indent-offset . 4)
;;     (web-mode-code-indent-offset . 4)
;;     (web-mode-css-indent-offset . 4)
;;     (js-indent-level . 2)
;;     (web-mode-enable-auto-pairing . t)
;;     (web-mode-enable-auto-expanding . t)
;;     (web-mode-enable-css-colorization . t)
;;     (indent-tabs-mode . t)
;;     (tab-width . 4)
;;     (web-mode-html-offset . 4)
;;     (web-mode-css-offset . 4)
;;     (web-mode-script-offset . 4)
;;     (web-mode-php-offset . 4)))

;; (leaf js-mode)

;; js-mode in Emacs 27 includes full support for syntax highlighting and indenting of JSX syntax.
;; See also: https://github.com/mooz/js2-mode
;; (leaf js2-mode
;;   :ensure t
;;   :custom
;;   (js2-basic-offset . 2)
;;   (js-switch-indent-offset . 2))

;; (leaf php-mode
;;   :ensure t)

;; (leaf json-mode
;;   :ensure t)

;; (leaf yaml-mode
;;   :ensure t
;;   :mode
;;   ("\\.yml$'"))

;; (leaf toml-mode
;;   :ensure t
;;   :mode
;;   ("\\.toml$'"))

;; (leaf glsl-mode
;;   :ensure t
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.shader$" . glsl-mode)))

;;------------------------------------
;; Javascript development environment
;;------------------------------------
;; (leaf npm
;;   :ensure t
;;   :init
;;   (require 'npm))

;; (leaf indium
;;   :doc "User Manual: https://indium.readthedocs.io/en/latest/index.html"
;;   :ensure t
;;   :config
;;   ;; (executable-find "indium")
;;   (add-to-list 'exec-path "/usr/local/bin/indium"))

;;------------------------------------
;; JUNK CODE
;;------------------------------------

(defun he-emacs-init-open ()
  "Open init.el file."
  (interactive)
  (message "Open init.el.")
  (find-file "~/.emacs.d/init.el"))

(defmacro when-terminal-in-darwin (&rest body)
  "Running Emacs on terminal of macOS?"
  (when (and (equal system-type 'darwin) (equal window-system nil))
    `(progn ,@body)))

;; org-ctrl-c-tabと被ることがその後に判明
(global-set-key (kbd "C-c C-i") 'he-emacs-init-open)
;; (global-set-key (kbd "C-c SPC") 'whitespace-cleanup)

;; Emacsの起動時間を計測したい時に使う
(global-set-key (kbd "C-c C-t")
                #'(lambda ()
                    (interactive)
                    (message (emacs-init-time))))

(adjust-pos-and-size-of-current-frame-to-center)

;; (defun he-emacs-init-reload ()
;;   "Reload init.el file."
;;   (interactive)
;;   (message "Try to reload init.el.")
;;   (load-file "~/.emacs.d/init.el"))

;; (defun he-emacs-init-compile ()
;;   "Byte compile init.el file."
;;   (interactive)
;;   (message "Try to byte-compile init.el.")
;;   (byte-compile-file "~/.emacs.d/init.el"))

;; (defun he-scratch-buffer ()
;;   "Switch *scratch* buffer."
;;   (interactive)
;;   (switch-to-buffer "*scratch*"))

;; (defun he-org-capture-open ()
;;   "Open capture.org file."
;;   (interactive)
;;   (find-file "~/Documents/org/capture.org"))

;; (defun he-org-publish ()
;;   "Run org-publish without skipping unmodified."
;;   (interactive)
;;   (org-publish "org-notes" t))

;; (setq default-frame-alist
;;     '(
;;       (width . 120)
;;       (height . 40)
;;       (top . 0)
;;       (left . 0)
;;       (font . "-*-MigMix 2M-normal-normal-normal-*-*-*-*-*-p-0-iso10646-1")))

;; (defun hiroakit/typeface (family size)
;;   "Set default typeface using `FAMILY' and `SIZE'."
;;   (when (member family (font-family-list))
;;     (message "hiroakit/typeface: %s founded." family)
;;  ;; (set-face-attribute 'default nil :family "MigMix 2M" :height 110)
;;  ;; フォントサイズを指定すると、text-scale-mode で大きさを変更できなくなる（文字サイズが固定になる）
;;  (set-fontset-font t 'japanese-jisx0208 (font-spec :family family :size nil))
;;  (set-fontset-font t 'japanese-jisx0212 (font-spec :family family :size nil))
;;  ;; (let* ((fontspec (font-spec :family family :size size))
;;  ;;     (fontconfig (format "%s-%d" family size))

;;  ;;     ;; Create fontset based on ASCII. Should use fontconfig. e.g. "fontfamily-fontsize"
;;  ;;     (fontset (create-fontset-from-ascii-font fontconfig nil family)))

;;  ;;   (set-fontset-font fontset 'unicode fontspec nil 'append)
;;  ;;   (set-frame-font fontset))
;;  ))

;; (defun hiroakit/osx-keychain-find-generic-password (service account)
;;   "Get password from OSX Keychain.
;; `SERVICE' is keychain item name.
;; `ACCOUNT' is user name."
;;   (let ((cmd (format "security find-generic-password -s %s -a %s -w" service account)))
;;  (shell-command-to-string cmd)))

;; Startup Emacs
;; (leaf startup
;;   :preface
;;   (defun hiroakit/text-scale-up ()
;;     (interactive)
;;  (text-scale-increase 1))
;;   (defun hiroakit/text-scale-down ()
;;     (interactive)
;;  (text-scale-decrease 1))
;;   (defun hiroakit/text-scale-reset ()
;;     (interactive)
;;  (text-scale-set 0))
;;   :hook
;;   (emacs-startup-hook . (lambda ()
;;                        (message "Run emacs-startup-hook")
;;                        ;; Gen Shin Gothic Monospace
;;                        ;; Myrica M
;;                        ;; MotoyaLCedar
;;                        ;; HackGen Console
;;                        ;; Sarasa Mono J
;; ;;						  (hiroakit/typeface "Sarasa Mono J" 14)))
;;                        (hiroakit/typeface "MigMix 2M" 14)))
;;   :bind
;;   (("C-<wheel-up>" . hiroakit/text-scale-up)
;;    ("C-<wheel-down>" . hiroakit/text-scale-down)
;;    ("<C-mouse-4>" . hiroakit/text-scale-up)
;;    ("<C-mouse-5>" . hiroakit/text-scale-down)
;;    ("M-0" . hiroakit/text-scale-reset)
;;    ("C-j" . goto-line)
;;    ("C-c t" . toggle-truncate-lines)
;;    ("C-c r" . rename-file))
;;   :custom
;;   ((inhibit-startup-screen . t)
;;    (inhibit-startup-message . t)
;;    (inhibit-startup-echo-area-message . t)
;;    (initial-scratch-message . nil)
;;    (cua-mode . t)
;;    (cua-enable-cua-keys . nil))
;;   :config
;;   (setq eol-mnemonic-dos "(CRLF)")
;;   (setq eol-mnemonic-mac "(CR)")
;;   (setq eol-mnemonic-unix "(LF)"))

;; (leaf paren
;;   :doc "Highlighting the corresponding brackets"
;;   :custom
;;   ((show-paren-style  . 'mixed))
;;   :hook
;;   (emacs-startup-hook . show-paren-mode))

;; (leaf expand-region
;;   :ensure t
;;   :bind
;;   (("C-S-f" . er/expand-region)
;;    ("C-S-d" . er/contract-region)))

;; (leaf multiple-cursors
;;   :doc "Conflict with helm M-x. See https://github.com/emacs-helm/helm/issues/960"
;;   :disabled t
;;   :bind
;;   (("C-S-c C-S-c" . mc/edit-lines)
;;    ("C-S->" . mc/mark-next-like-this)
;;    ("C-S-<" . mc/mark-previous-like-this)
;;    ("C-c C-<" . mc/mark-all-like-this)))

;;-------------------------
;; popwin
;;-------------------------

;; (with-delayed-execution
;;   (message "Install popwin...")
;;   (add-to-list 'load-path (locate-user-emacs-file "el-get/popwin"))
;;   (autoload-if-found '(popwin-mode) "popwin" nil t)
;;   (popwin-mode 1))

;;-------------------------
;; Dired
;;-------------------------

;; (with-eval-after-load 'dired
;;   ;; config
;;   (setq dired-auto-revert-buffer nil)
;;   (setq dired-dwim-target t)
;;   (setq dired-hide-details-hide-symlink-targets nil)
;;   (setq dired-listing-switches "-alh")
;;   (setq dired-recursive-copies 'always)
;;   (setq dired-use-ls-dired nil)

;;   ;; hook
;;   (add-hook 'dired-mode-hook #'(lambda () (display-line-numbers-mode -1))))

;; (provide 'init)

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(custom-safe-themes
;;    '("02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "dde643b0efb339c0de5645a2bc2e8b4176976d5298065b8e6ca45bc4ddf188b7" default))
;;  '(package-selected-packages
;;    '(modus-themes orderless consult vertico doom-modeline leaf-tree leaf-convert blackout el-get hydra leaf-keywords)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("250007c5ae19bcbaa80e1bf8184720efb6262adafa9746868e6b9ecd9d5fbf84" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "dde643b0efb339c0de5645a2bc2e8b4176976d5298065b8e6ca45bc4ddf188b7" "bfc0b9c3de0382e452a878a1fb4726e1302bf9da20e69d6ec1cd1d5d82f61e3d" default))
 '(package-selected-packages
   '(consult-eglot eglot company neotree undo-tree orderless consult vertico doom-modeline leaf-tree leaf-convert blackout el-get hydra leaf-keywords)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
