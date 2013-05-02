(defun set-whitespace-options ()
  ;; Set whitespace settings
  (require 'whitespace)
  (setq whitespace-style
    '(trailing
          empty
          space-after-tab
          space-before-tab
          lines-tail
          indentation face))
  (setq whitespace-action '(auto-cleanup))

  ;; Start up whitespace mode when it makes sense
  (add-hook 'python-mode-hook 'whitespace-mode)
  (add-hook 'haskell-mode-hook 'whitespace-mode)

  (add-hook 'c-mode-common-hook 'whitespace-mode)
  )

(set-whitespace-options)

(provide 'whitespace-config)
