(when (require 'org-install nil t))
(when (require 'org-habit nil t))

;; ファイルは折り畳んだ状態で開く
(setq org-startup-truncated nil) 

;; return でリンクを追う
;; (setq org-return-follows-link t) 

;; ファイルの拡張子が org だった場合，org-modeを起動する
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; 必要最低限の「*」のみ表示する
;; orgファイルに #+STARTUP: hidestars と記述しなくて済むようになる
(setq org-hide-leading-stars t)

;; orgファイルが入っているディレクトリ
(setq org-directory "~/org/")

;; org-default-notes-fileのファイル名
(setq org-default-notes-file (concat org-directory "notes.org"))

;; アジェンダ表示対象のファイル
(setq org-agenda-files (list org-directory))

;;; Libre Office 
;; Org-mode with Libre Office Writer
(setq org-export-odt-convert-processes 
      '(("LibreOffice" "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i")
        ("unoconv" "unoconv -f %f -o %d %i")))

;;; Org-mode :: TODO
;; C-c C-tを押すと、どれを選択するかとミニバッファが開く
;;
;; 見栄えの関係で，WAITにする．本来ならWAITINGとすべき
;; 見栄えの関係で，VERIにする．本来ならVERIFYとすべき
;; 見栄えの関係で，CANCにする．本来ならCANCELEDとすべき
(setq org-todo-keywords 
      '((sequence "TODO(t)" 
                  "VERI(y)" 
                  "WAIT(w)" 
                  "CANC(c)" "|" "DONE(d)")))

;; Taskの属性名につける装飾
(setq org-todo-keyword-faces
      '(("TODO"     . org-warning)
        ("CANCELED" . shadow)))

;; TAGの設定
(setq org-tag-alist 
      '(("OFFICE" . ?o)
        ("HOME" . ?h)
        ("PROJECT" . ?p)
        ("READING" . ?r)
        ("MAC" . ?a)
        ("DVD" . ?b)
        ("LUNCHTIME" . ?l)
        ("CONFERENCE" . ?m)
        ("DESIGN" . ?d)
        ("CODING" . ?c)))

;;; Org-mode :: Agenda

;; アジェンダ表示で下線を用いる
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
(setq hl-line-face 'underline)

;;; org-agenda-custom-command

(org-add-agenda-custom-command
 '("A" "Review this week"
   ((agenda ""
     ((org-agenda-time-grid nil)
      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
      (org-agenda-sorting-strategy '(time-up todo-state-up priority-down))
      (org-scheduled-past-days 14)
      (org-deadline-warning-days 14)))
    (todo "WAIT")
    (tags-todo "OFFICE")
    (tags-todo "HOME")
    (tags-todo "COMPUTER")
    (tags-todo "DVD")
    (tags-todo "READING"))))

(org-add-agenda-custom-command
 '("B" "Review today"
   ((agenda "" 
    ((tags "OFFICE")
     (org-agenda-span 'day)
     (org-deadline-warning-days 0)
     (org-agenda-sorting-strategy '(time-up todo-state-up priority-down))))
    (todo "WAIT"))))

(org-add-agenda-custom-command
 '("D" "デッドライン付きタスクを表示" 
   ((agenda "" ((org-agenda-time-grid nil)
                (org-deadline-warning-days 365) 
                (org-agenda-entry-types '(:deadline)))))))

(org-add-agenda-custom-command
 '("N" . "不明確なタスク"))

(org-add-agenda-custom-command
 '("NS" "スケジュール未定のタスク" 
   ((todo "TODO"
              ((org-agenda-overriding-header "No due date")
               (org-agenda-skip-function
                '(org-agenda-skip-entry-if 'scheduled 'deadline)))))

   ;;   ((agenda "" ((org-agenda-overriding-header "未定のタスク")
;;                (todo "TODO")
                ;;(org-deadline-warning-days 365)
                ;; うまく効かない
                ;;(org-agenda-skip-function 
                ;; '(org-agenda-skip-entry-if 'todo '("WAIT")))
                ;; (org-agenda-skip-function 
                ;;  '(org-agenda-skip-entry-if 'todo 'done))
                ;; (org-agenda-skip-function 
                ;;  '(org-agenda-skip-entry-if 'scheduled 'deadline))
                ))

(org-add-agenda-custom-command
 '("c" "2週間分の予定を表示"
   ((agenda "" ((org-agenda-span 14)
                (org-agenda-time-grid nil)
                (org-scheduled-past-days 10)
                (org-deadline-warning-days 10)
                (org-agenda-repeating-timestamp-show-all nil)
                (org-agenda-skip-function 
                 '(org-agenda-skip-entry-if 'todo 'done)))))))

;; TODOステータスが"WAIT"であるタスクを列挙する
(org-add-agenda-custom-command
 '("W" "Waiting for a response task list" 
   ((todo "WAIT"))))

;; タグ"PROJECT"で，かつTODOステータスがTODOもしくはWAITのタスクを列挙する
(org-add-agenda-custom-command
 '("P" "Projects (TODO & WAIT only)" 
   ((tags "+PROJECT/!+TODO|+WAIT"))))

;; タグ"HOME"で，かつTODOステータスがTODOもしくはWAITのタスクを列挙する
(org-add-agenda-custom-command
 '("H" "HOME (TODO & WAIT only)" 
   ((tags "+HOME/!+TODO|+WAIT"))))

;; タグ"READING"で，かつTODOステータスがTODOもしくはWAITのタスクを列挙する
(org-add-agenda-custom-command
 '("R" "Reading task (TODO & WAIT only)" 
   ((tags "+READING/!+TODO|+WAIT"))))

;;; org-refile
(setq org-refile-targets
      (quote (("~/org/notes.org" :level . 1)
              ("~/org/private.org" :level . 1)
              ("~/org/jugemu.org" :level . 1)
              ("~/org/book.org" :level . 1)
              ("~/org/tools/windows.org" :level . 1)
              ("~/org/tools/emacs.org" :level . 1))))

;;; org-capture
;; 設定に使っている値は，The Org Manual 9.1.3 Capture templatesを参照せよ
(setq org-capture-templates
   `(
     ("t" "TODO項目をInboxに追加する" entry
       (file+headline nil "INBOX") "** TODO %?\n\t作成日: %T")
     ("r" "読みたい本をReadingに追加する" entry
       (file+headline nil "Reading") "** TODO %?\n\t")
     ("w" "英単語をEnglish > 英単語に追加する" checkitem
       (file+olp org-default-notes-file "English" "英単語") "- [ ] %?\n\t")
))

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

