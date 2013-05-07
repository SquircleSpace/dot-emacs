(defun set-objc-options ()
  (require 'auto-complete-config)
  (require 'autocomplete-config)

  (add-hook 'objc-mode-hook 'flymake-mode)
  (add-to-list 'ac-modes 'objc-mode)
  )

(set-objc-options)

(provide 'objc-config)
