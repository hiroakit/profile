
;;; Code:

(defun hp-show-core-conf ()
  "init.elを開く."
  (interactive)
  (switch-to-buffer (find-file-noselect hp-core-conf)))

(defun hp-measure-emacs-initialization-time ()
  "Emacsの初期化にかかる秒数を計測する."
  (message "Emacs initialization time: %.3f sec"
             (float-time (time-subtract after-init-time before-init-time))))

(defun add-org-task-to-reminder ()
  "orgのタスクをリマインダー.appに登録する."
  (interactive)
  (when (eq major-mode 'org-mode)
    (setq reminder-list-name "リマインダー")
    (setq element (org-element-at-point))
    (setq title (org-element-property :title element))
    (setq command (format "reminders add %s %s" reminder-list-name title))
    (shell-command-to-string command)))

;; (defun hp-org-task4 ()
;;   "hoge"
;;   (interactive)
;;   (when (eq major-mode 'org-mode)
;;      (let ((ele (org-element-at-point)))
;;        (message (org-element-property :raw-value ele)))))

;; (defun hp-org-task2 ()
;;   "hoge"
;;   (interactive)
;;   (when (eq major-mode 'org-mode)
;;     (org-map-entries
;;      '(progn (if (equal (org-entry-get (point) "TODO") "TODO")
;;                  (message (org-entry-get (point) ":raw-value"))))))) 

(defun hp-create-temp-org-buffer ()
  "Get new temporary org-mode buffer."
  (interactive)
  ;; バッファ *temp org* を作成
  (switch-to-buffer (generate-new-buffer "*temp org*"))  
  ;; org-modeに切り替える
  (org-mode))

(defun hp-insert-current-date-text ()
  "Insert current date."  
  (interactive) 
  (insert (format-time-string "%Y/%m/%d"))) 
(defun hp-insert-current-year-text ()
  "Insert current year."  
  (interactive) 
  (insert (format-time-string "%Y"))) 
(defun hp-insert-current-month-text ()
  "Insert current month."  
  (interactive) 
  (insert (format-time-string "%m"))) 
(defun hp-insert-current-day-text ()
  "Insert current day."  
  (interactive) 
  (insert (format-time-string "%d"))) 

(defun hp-open-current-directory ()
  "Open current directory."
  (interactive)
  (shell-command (concat "open .")))

(defun hp-move-frame-line ()
  "Move frame line."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action
              (read-key-sequence-vector (format "size[%dx%d]"
                                                (window-width)
                                                (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?k)
               (enlarge-window dy))
              ((= c ?j)
               (shrink-window dy))
              ;; otherwise
              (t
               (let ((last-command-char (aref action 0))
                     (command (key-binding action)))
                 (when command
                   (call-interactively command)))
               (message "Quit")
                              (throw 'end-flag t)))))))

(provide 'hp-utility)

;;; hp-utility.el ends here
