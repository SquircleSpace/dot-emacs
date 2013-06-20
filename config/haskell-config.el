(defun set-haskell-options ()
  (require-package 'haskell-mode)
  (require-package 'flymake-haskell-multi)

  ;; Set up autocomplete
  (require 'autocomplete-config)
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/"))
  (require 'haskell-ac)
  (add-to-list 'ac-modes 'haskell-mode)

  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'font-lock-mode)
  (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

  ;; Set up flymake
  (require 'flymake-config)
  (add-hook 'haskell-mode-hook 'flymake-haskell-multi-load)

  ;; Set up flyspell
  (require 'flyspell-config)
  (add-hook 'haskell-mode-hook 'flyspell-prog-mode))

(set-haskell-options)

(provide 'haskell-config)
