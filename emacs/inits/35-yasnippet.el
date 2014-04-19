;;; yasnippet
;; (when (require 'yasnippet nil t))

;;; ユーザ定義のスニペットを保存するフォルダ
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets" ;; 作成するスニペットはここに入る
        ))
(yas-global-mode 1)

;;; 操作とキーボード
(custom-set-variables '(yas-trigger-key "TAB"))
(define-key yas-minor-mode-map 
  (kbd "C-x i i") 'yas-insert-snippet) ;; 既存スニペットを挿入する
(define-key yas-minor-mode-map 
  (kbd "C-x i n") 'yas-new-snippet) ;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map 
  (kbd "C-x i v") 'yas-visit-snippet-file) ;; 既存スニペットを閲覧・編集する
