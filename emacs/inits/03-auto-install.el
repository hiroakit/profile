;;; auto install

(when (require 'auto-install nil t) ;auto-install.elがある場合
  (setq auto-install-directory "~/.emacs.d/site-lisp/auto-install/")
  (auto-install-compatibility-setup) ; 開発者曰く，互換性確保のために必要とのこと       
)
