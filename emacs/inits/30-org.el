(when (require 'org-install nil t))
(when (require 'org-habit nil t))

;; ファイルは折り畳んだ状態で開く
(setq org-startup-truncated nil) 

;; return でリンクを追う
;; (setq org-return-follows-link t) 

;; ファイルの拡張子が org だった場合，org-modeを起動する
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; org-modeで強調表示を可能にする
(add-hook 'org-mode-hook 'turn-on-font-lock)

;; 必要最低限の「*」のみ表示する
;; orgファイルに #+STARTUP: hidestars と記述しなくて済むようになる
(setq org-hide-leading-stars t)

;; org-default-notes-fileのディレクトリ
(setq org-directory "~/org/")

;; org-default-notes-fileのファイル名
(setq org-default-notes-file (concat org-directory "notes.org"))

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

;; アジェンダ表示対象のファイル
(setq org-agenda-files (list org-directory))

;; アジェンダ表示で下線を用いる
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
(setq hl-line-face 'underline)

;; カスタムアジェンダコマンド
;; 
;; 関連資料
;; - http://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.htm
;; - http://members.optusnet.com.au/%7Echarles57/GTD/mydotemacs.txt
;; 
;; 使い方
;; C-c a の後に d, c, E などを押す
(setq org-agenda-custom-commands
      '(("d" "Upcoming deadlines" agenda "" 
         ((org-agenda-time-grid nil)
          (org-deadline-warning-days 365) 
          (org-agenda-entry-types '(:deadline))))
        ("c" "Calendar"
         ((agenda "" (
                      (org-agenda-ndays 'week)
                      (org-agenda-start-on-weekday 1)
                      (org-agenda-repeating-timestamp-show-all nil)
;;                      (org-agenda-skip-function 
;;                       '(org-agenda-skip-entry-if 'deadline 'scheduled)) 
                      (org-agenda-entry-types '(:timestamp :sexp))
                      ))))

        ("E" . "Enginnering tasks")
        ("EO" "Engineering tasks on Office"
         ((tags-todo "OFFICE&DESIGN")
          (tags-todo "OFFICE&CODING")))
        ("EH" "Engineering tasks on Home"
         ((agenda "" ((org-agenda-ndays 14)
                      (org-agenda-start-on-weekday nil)
                      ))
          (tags-todo "HOME&CONFERENCE")
          (tags-todo "HOME&DESIGN")
          (tags-todo "HOME&CODING")))
        ("D" "Daily Action List"
         ((agenda "" ((org-agenda-ndays 1)
                      (org-agenda-sorting-strategy
                       (quote ((agenda time-up priority-down tag-up))))
                      (org-deadline-warning-days 0)
                      ))))
        ;; ("c" "Weekly schedule" agenda ""
        ;;  ((org-agenda-ndays 7)
        ;;   (org-agenda-repeating-timestamp-show-all t)
        ;;   (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))) 
        ;; ("H" "Homework" 
        ;;  ((tags-todo "HOME")))
        
        ;; タグ'PROJECT'が付いているもので，TODOあるいはWAIT状態にあるタスクを表示する
        ("P" "Projects (TODO & WAIT only)" 
         ((tags "PROJECT/!+TODO|+WAIT")))

        ("H" "Office and Home Lists"
         ((agenda)
          (tags-todo "OFFICE")
          (tags-todo "HOME")
          (tags-todo "COMPUTER")
          (tags-todo "DVD")
          (tags-todo "READING"))))
)

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

