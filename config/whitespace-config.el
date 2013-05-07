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
  (let ((hooks '(emacs-lisp-mode-hook python-mode-hook haskell-mode-hook
                 c-mode-common-hook java-mode-hook)))
    (dolist (hook hooks)
      (add-hook hook 'whitespace-mode)))
  )

(set-whitespace-options)

(provide 'whitespace-config)
