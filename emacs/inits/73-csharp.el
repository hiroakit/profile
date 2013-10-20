;; C# mode
(when (locate-library "csharp-mode")
  (autoload 'csharp-mode' "csharp-mode" "" t)
)

(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))
