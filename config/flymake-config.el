(require-package 'flymake 'flymake-cursor)

;; Configure flymake
(eval-after-load 'flymake
  '(setq flymake-no-changes-timeout 5.0)) ; Don't bother me while typing.

(provide 'flymake-config)
