(require 'package)

(defvar HP_MELPA_URL "http://melpa.milkbox.net/packages/")
(defvar HP_MARMALADE_URL "http://marmalade-repo.org/packages/")
(add-to-list 'package-archives (cons "melpa" HP_MELPA_URL))
(add-to-list 'package-archives (cons "marmalade" HP_MARMALADE_URL))
(package-initialize)

(require 'cl)

(defvar installing-package-list
  '(
    ;; ここに使っているパッケージを書いていく
    scala-mode2
    yasnippet
    auto-complete
    foreign-regexp
    web-mode
    js2-mode
    csharp-mode
    cmake-mode
    ruby-mode
    ruby-additional
    ;; ruby-electric ELPAのバージョンは調子が良くない模様
    ruby-block
    )
)

(let ((not-installed 
       (loop for x in installing-package-list
             when (not (package-installed-p x)) collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist 
        (pkg not-installed)
        (package-install pkg))))
