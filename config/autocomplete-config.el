(require 'package-config)

(defun set-autocomplete-options ()
  (require-package 'auto-complete)
  (require-package 'auto-complete-clang-async)
  (require-package 'yasnippet)
  (require 'auto-complete-config)
  (require 'auto-complete-clang-async)
  (require 'yasnippet)

  ;; Initalize autocomplete
  (ac-config-default)
  (ac-flyspell-workaround)

  ;; Setup clang-async
  (defun ac-cc-mode-setup ()
    (yas-minor-mode-on) ; Strange things happen if yas-minor-mode isn't on
    (setq ac-sources '(ac-source-clang-async)) ; We only need clang-async
    (ac-clang-launch-completion-process)) ; Need to explicitly launch process

  (setq ac-clang-complete-executable "~/.emacs.d/clang-complete")
  (add-to-list 'ac-sources 'ac-source-clang-async)
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'objc-mode-hook 'ac-cc-mode-setup)
  )

(set-autocomplete-options)

(provide 'autocomplete-config)
