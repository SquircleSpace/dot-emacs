(defun set-flymake-options ()
  (require-package 'flymake 'flymake-cursor 'auto-complete-clang-async)

  (require 'auto-complete-clang-async)
  (require 'flymake)
  (push '("\\.\\(?:c\\(?:xx\\|pp\\|\\+\\+\\)?\\|CC\\|h\\|hpp\\|m\\)\\'"
          ac-clang-syntax-check)
        flymake-allowed-file-name-masks)

  ;; Start up flymake mode when it makes sense
  (let ((modes '(c-mode-common-hook)))
    (dolist (mode modes)
      (add-hook mode 'flymake-mode)))
  )

(set-flymake-options)

(provide 'flymake-config)
