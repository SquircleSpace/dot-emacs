(require 'package-config)

(defun set-haskell-options ()
  (require-package 'haskell-mode)

  (eval-after-load 'auto-complete
    '(progn
       (add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/"))
       (require 'haskell-ac)
       (require 'auto-complete-config)
       (add-to-list 'ac-modes 'haskell-mode)
       )
    )
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'font-lock-mode)
  (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

  )

(set-haskell-options)

(provide 'haskell-config)
