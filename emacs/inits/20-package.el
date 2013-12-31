(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
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
    csharp-mode
    cmake-mode
    ruby-mode
    ruby-additional
    ;; ruby-electric ELPAのバージョンは調子が良くない模様
    ruby-block
    ))

(let ((not-installed (loop for x in installing-package-list
                            when (not (package-installed-p x))
                            collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
        (package-install pkg))))
