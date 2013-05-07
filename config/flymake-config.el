(defun set-flymake-options ()
  (require-package 'flymake 'flymake-cursor 'auto-complete-clang-async)
  (require 'auto-complete-clang-async)
  (require 'flymake)

  (setq flymake-no-changes-timeout 5.0) ; Don't bother me while typing.

  (push '("\\.\\(?:c\\(?:xx\\|pp\\|\\+\\+\\)?\\|CC\\|h\\|hpp\\|m\\)\\'"
          (lambda () (ac-clang-launch-completion-process)
            (ac-clang-syntax-check)))
        flymake-allowed-file-name-masks)

  ;; Start up flymake mode when it makes sense
  (let ((modes '(c-mode-common-hook)))
    (dolist (mode modes)
      (add-hook mode 'flymake-mode)))
  )

(set-flymake-options)

(provide 'flymake-config)
