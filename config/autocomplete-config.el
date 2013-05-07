(defun set-autocomplete-options ()
  (require-package 'auto-complete)
  ; Unfortunately, this package doesn't include the binary we need.
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

  ;; Tell clang-async where to look for clang-complete
  (setq ac-clang-complete-executable
        "~/.emacs.d/config/autocomplete-config/clang-complete")
  ;; Build it if it isn't there
  (unless (file-exists-p ac-clang-complete-executable)
    (call-process "make" nil nil nil "-C"
                   "~/.emacs.d/config/autocomplete-config/")
    (unless (file-exists-p ac-clang-complete-executable)
      (error "Couldn't build clang-complete")))
    
  (add-to-list 'ac-sources 'ac-source-clang-async)
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'objc-mode-hook 'ac-cc-mode-setup)
  )

(set-autocomplete-options)

(provide 'autocomplete-config)
