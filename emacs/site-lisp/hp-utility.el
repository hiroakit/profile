
;;; Code:

(defun hp-open-current-directory ()
  "Open current directory."
  (interactive)
  (shell-command (concat "open .")))

(provide 'hp-utility)

;;; hp-utility.el ends here
