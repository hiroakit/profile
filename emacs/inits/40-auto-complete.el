(when (require 'auto-complete-config nil t) ;auto-complete-config.elがある場合に以下を実行する.
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/auto-complete/ac-dict") ;辞書ファイルがあるフォルダを設定する．
  (ac-config-default)) ;補完候補の情報源の読み込み．

;; auto-completeの初期化設定
(when (require 'ac-company nil t) ;ac-company.elがある場合に以下を実行する.
  (setq ac-auto-start nil) ; 補完機能が自動で起動することを禁止する.
  (ac-set-trigger-key "TAB") ; 補完機能を起動するキーバインドをタブに設定する．
  (setq ac-candidate-max 20) ; 補完候補に出す表示件数を20件までとする．
  (global-auto-complete-mode t) ; 対象の全てで補完を有効にする．

  ;; (add-hook 'AC-mode-hook
  ;;           (set-face-background 'ac-selection-face "gray35"))

  ;; 補完ウィンドウ内のキーバインド
  (define-key ac-completing-map (kbd "C-n") 'ac-next) ;次の変換候補にカーソルを移動する.
  (define-key ac-completing-map (kbd "C-p") 'ac-previous) ;前の変換候補にカーソルを移動する．
  (define-key ac-completing-map (kbd "ESC") 'ac-stop) ;現在の変換候補の表示をやめる．
)
