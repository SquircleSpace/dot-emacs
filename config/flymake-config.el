(defun set-flymake-options ()
  (require-package 'flymake 'flymake-cursor)

  ;; Configure flymake
  (eval-after-load 'flymake
    '(setq flymake-no-changes-timeout 5.0)) ; Don't bother me while typing.
  )

(set-flymake-options)

(provide 'flymake-config)
