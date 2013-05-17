(defun set-flymake-options ()
  (require-package 'flymake 'flymake-cursor)

  ;; Configure flymake
  (eval-after-load 'flymake
    '(setq flymake-no-changes-timeout 5.0)) ; Don't bother me while typing.

  ;; Setup flymake for c modes
  (eval-after-load 'cc-mode
    '(progn
       (require-package 'auto-complete-clang-async)
       (require 'auto-complete-clang-async)

       ; Claim to support c files
       (push '("\\.\\(?:c\\(?:xx\\|pp\\|\\+\\+\\)?\\|CC\\|h\\|hpp\\)\\'"
               (lambda ()
                 (unless ac-clang-completion-process
                   (ac-clang-launch-completion-process))
                 (ac-clang-syntax-check)))
             flymake-allowed-file-name-masks)

       (add-hook 'c-mode-common-hook 'flymake-mode)))
  )

(set-flymake-options)

(provide 'flymake-config)
