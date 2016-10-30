
;;; Code:

(defun hp-create-temporary-org-buffer ()
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

(provide 'hp-utility)

;;; hp-utility.el ends here
