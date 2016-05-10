(defun hp-initialize-helm-gtags-keybinding ()
  "helm-gtags-modeのキーバインディングを初期化します"

  ;; (setq helm-gtags-suggested-key-mapping t)
  ;; (setq helm-gtags-prefix-key "\C-c")
  
  ;; helm-gtags-find-tag	           関数の定義場所の検索
  ;; helm-gtags-find-rtag              関数や使用箇所の検索
  ;; helm-gtags-find-symbol            変数の使用箇所の検索
  ;; helm-gtags-pop-stack              タグジャンプした箇所からひとつ戻る
  ;; helm-gtags-parse-file             関数の定義一覧
  ;; helm-gtags-tags-in-this-function  関数内のタグ一覧
  (define-key helm-gtags-mode-map (kbd "C-c C-t") 'helm-gtags-find-tag)
  (define-key helm-gtags-mode-map (kbd "C-c C-r") 'helm-gtags-find-rtag)
  (define-key helm-gtags-mode-map (kbd "C-c C-s") 'helm-gtags-find-symbol)
  (define-key helm-gtags-mode-map (kbd "C-c C-p") 'helm-gtags-pop-stack)
  (define-key helm-gtags-mode-map (kbd "C-c C-f") 'helm-gtags-parse-file)
  (define-key helm-gtags-mode-map (kbd "C-c C-a") 'helm-gtags-tags-in-this-function))

(with-eval-after-load 'helm-gtags
  (hp-initialize-helm-gtags-keybinding))

(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)

