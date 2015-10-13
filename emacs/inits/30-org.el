(when (require 'org-install nil t))
(when (require 'org-habit nil t))

(defvar hp-org-private-dir 
  "~/org/" 
  "私的のorgファイル格納先ディレクトリ")
(defvar hp-org-work-dir 
  "~/work/" 
  "仕事のorgファイルの格納先ディレクトリ")
(defvar hp-org-refile-targets 
  '(("~/org/inbox.org" :level . 1)
    ("~/org/private.org" :level . 1)
    ("~/org/book.org" :level . 1)
    ("~/work/work.org" :level . 2)
    ) 
  "org-refileの対象")
(defvar hp-org-capture-templates
   '(("t" "TODOをInboxに追加する" entry
      (file+headline org-default-notes-file "Inbox") "** TODO %?\n\t作成日: %U\n\t")
     ("r" "興味のある本を追加する" entry
      (file+headline "~/org/book.org" "Inbox") "** TODO %?\n\t")
     ("w" "英単語をEnglish > 英単語に追加する" checkitem
      (file+olp org-default-notes-file "English" "英単語") "- [ ] %?\n\t"))
   "org-captureテンプレート")

;; ファイルは折り畳んだ状態で開く
(setq org-startup-truncated nil) 

;; ファイルの拡張子が org だった場合，org-modeを起動する
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; 必要最低限の「*」のみ表示する
;; orgファイルに #+STARTUP: hidestars と記述しなくて済むようになる
(setq org-hide-leading-stars t)

;; orgファイルが入っているディレクトリ
(setq org-directory hp-org-private-dir)

;; org-default-notes-fileのファイル名
(setq org-default-notes-file (concat org-directory "inbox.org"))

;; アジェンダ表示対象のファイル 
;; (ディレクトリを指定すると、そこに入っている全てのファイルが対象となる)
(setq org-agenda-files (list org-directory hp-org-work-dir))

;;; Libre Office 
;; Org-mode with Libre Office Writer
(setq org-export-odt-convert-processes 
      '(("LibreOffice" "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i")
        ("unoconv" "unoconv -f %f -o %d %i")))

;;; Org-modeのTODOステータス(C-c C-tでミニバッファが開く)
(setq org-todo-keywords 
      '((sequence "TODO(t)" "|" "DONE(d!)")
        (sequence "WAITING(w@/!)" "|")
        (sequence "|" "CANCELED(c@/!)")
        ))

;; Taskの属性名につける装飾
(setq org-todo-keyword-faces
      '(("TODO"     . org-warning)
        ("CANCELED" . shadow)))

;; TAGの設定
(setq org-tag-alist 
      '(
        ("@HOME" . ?h)
        ("@OFFICE" . ?o)
        ("@TSUTAYA" . ?t)
        ("@ITOYOKADO" . ?i)
        ("@CLEANERS" . ?x)
        ("@POSTOFFICE" .?y)
        ("@BANK" . ?z)
        ("@LUNCHTIME")
        ("PHONE" . ?p)
        ("MAIL" . ?m)
        ("CONFERENCE" . ?c)
        ("TALKING")
        ("READING" . ?r)
        ("WATCHING" . ?w)
       ))

;;; Org-mode :: Agenda

;; アジェンダ表示で下線を用いる
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
(setq hl-line-face 'underline)

;;; org-agenda-custom-command
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
 '("O" "Office (TODO & WAITING only)" 
   ((tags "+OFFICE/!+TODO|+WAITING"))))

;; タグ"HOME"で，かつTODOステータスがTODOもしくはWAITINGのタスクを列挙する
(org-add-agenda-custom-command
 '("H" "HOME (TODO & WAITING only)" h
   ((tags "+HOME/!+TODO|+WAITING"))))

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
    (tags-todo "OFFICE")
    (tags-todo "HOME")
    (tags-todo "COMPUTER")
    (tags-todo "DVD")
    (tags-todo "READING"))))

;;; org-refile
(setq org-refile-targets hp-org-refile-targets)

;;; org-capture
(setq org-capture-templates hp-org-capture-templates)

;; コードハイライト
(setq org-src-fontify-natively t)
(add-to-list 'org-src-lang-modes '("csharp" . csharp))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (perl . t)
   (python . t)
   (js . t)))

;; org-mode-hook
(add-hook 'org-mode-hook 
          (lambda () 
            (local-set-key (kbd "C-c c") 'hp-show-org-conf)))

;; org-agenda-mode-hook
(add-hook 'org-agenda-mode-hook
          (lambda () 
            (local-set-key (kbd "C-c c") 'hp-show-org-conf)))

