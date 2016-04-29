;; undoの履歴をウィンドウを閉じても保持する
(when (require 'undohist nil t)
  (undohist-initialize))

;; undoの樹形図を表示する C-x u
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))
