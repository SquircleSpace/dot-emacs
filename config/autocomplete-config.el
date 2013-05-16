(defun set-autocomplete-options ()
  (require-package 'auto-complete)
  (require 'auto-complete-config)

  ;; Initalize autocomplete
  (ac-config-default)
  (ac-flyspell-workaround)
  (ac-linum-workaround)

  ;; No point in setting up clang-complete if we don't need it
  (eval-after-load 'cc-mode
    '(progn
       ;; Unfortunately, this package doesn't include the binary we need.
       (require-package 'auto-complete-clang-async)
       (require-package 'yasnippet)
       (require 'auto-complete-clang-async)
       (require 'yasnippet)

       ;; Tell clang-async where to look for clang-complete
       (setq ac-clang-complete-executable
             "~/.emacs.d/config/autocomplete-config/clang-complete")
       ;; Build it if it isn't there
       (unless (file-exists-p ac-clang-complete-executable)
         (call-process "make" nil nil nil "-C"
                       "~/.emacs.d/config/autocomplete-config/")
         (unless (file-exists-p ac-clang-complete-executable)
           (error "Couldn't build clang-complete")))

       ;; Setup clang-async
       (defun ac-cc-mode-setup ()
         (yas-minor-mode-on) ; Strange things happen if yas-minor-mode isn't on
         (setq ac-sources '(ac-source-clang-async ac-source-yasnippet))
         ;; Need to explicitly launch process
         (unless ac-clang-completion-process
           (ac-clang-launch-completion-process)))

       (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)))
  )

(set-autocomplete-options)

(provide 'autocomplete-config)
