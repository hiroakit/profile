;;; auto-complete
;;; hp is prefix. It full name is hiroaki-profile.
;;; ac is prefix. It full name is auto-complete.

(when (require 'auto-complete nil t))
(when (require 'auto-complete-config nil t))
(ac-config-default)

;;; 定数宣言
(defvar hp-ac-dict-root-uri 
  "~/.emacs.d/dict/ac-dict"
  "auto-completeのメジャーモード用辞書が置いてあるフォルダのパス")
(defvar hp-ac-user-dict-file-uri 
  ""
  "auto-completeのユーザ定義辞書のパス") 
(defvar hp-ac-cache-uri 
  "~/ac-comphist.dat"
  "auto-completeの補完履歴をキャッシュとして残すために使うファイルのパス")

;;; 辞書の在処
(add-to-list 'ac-dictionary-directories hp-ac-dict-root-uri) 
;;(add-to-list 'ac-user-dictionary-files hp-ac-user-dict-file-uri) 

;;; 補完履歴のキャッシュ保存先
(setq ac-comphist-file hp-ac-cache-uri) 

;;; 補完候補の表示はユーザが明示的に行う
(setq ac-auto-start nil)
(ac-set-trigger-key "TAB")

;;; 補完候補は0.5秒以内に表示し，20個まで表示する
(setq ac-auto-show-menu 0.5)
(setq ac-menu-height 20)

;;; 補完候補の選択操作とキーボード
(setq ac-use-menu-map t)
(define-key ac-menu-map (kbd "C-n") 'ac-next)
(define-key ac-menu-map (kbd "C-p") 'ac-previous)
(define-key ac-completing-map (kbd "ESC") 'ac-stop)

;;; elpaでインストールしたauto-completeでは動作確認したが、
;;; 効果を確認するには至らず。継続調査の必要あり。
;;(global-auto-complete-mode t)

;;; 常にYASnippetを補完候補に
;; (add-to-list 'ac-sources 'ac-source-yasnippet) 
;; (delq 'ac-source-yasnippet ac-sources)
