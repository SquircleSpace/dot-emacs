(defun set-objc-options ()
  (require 'autocomplete-config)
  (require 'flymake-config)
  (require-package 'adaptive-wrap)

  (add-hook 'objc-mode-hook 'visual-line-mode)
  (add-hook 'objc-mode-hook 'adaptive-wrap-prefix-mode)
  (add-hook 'objc-mode-hook (lambda ()
                              (setq adaptive-wrap-extra-indent 4)))
  (add-to-list 'ac-modes 'objc-mode)
  )

(set-objc-options)

(provide 'objc-config)
