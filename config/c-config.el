(defvar c-flymake-initalized nil
  "Whether flymake has been set up for c like modes.")

(defvar c-autocomplete-initalized nil
  "Whether autocomplete has been set up for c like modes.")

(defun init-c-flymake ()
  (unless c-flymake-initalized
    (setq c-flymake-initalized t)
    (require-package 'auto-complete-clang-async)
    (require 'auto-complete-clang-async)

    ;; Claim to support c files
    (push '("\\.\\(?:c\\(?:xx\\|pp\\|\\+\\+\\)?\\|CC\\|h\\|hpp\\)\\'"
            (lambda ()
              (unless ac-clang-completion-process
                (ac-clang-launch-completion-process))
              (ac-clang-syntax-check)))
          flymake-allowed-file-name-masks))

  (flymake-mode))

(defun init-c-autocomplete ()
  (unless c-autocomplete-initalized
    (setq c-autocomplete-initalized t)
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
        (error "Couldn't build clang-complete"))))

  ;; Setup clang-async
  (yas-minor-mode-on) ; Strange things happen if yas-minor-mode isn't on
  (setq ac-sources '(ac-source-clang-async ac-source-yasnippet))
  ;; Need to explicitly launch process
  (unless ac-clang-completion-process
    (ac-clang-launch-completion-process)))

(defun set-c-options ()

  (setq c-default-style
        (quote
         ((c-mode . "stroustrup")
          (c++-mode . "stroustrup")
          (java-mode . "java")
          (awk-mode . "awk")
          (other . "gnu"))))

  (require-package 'adaptive-wrap)

  (add-hook 'c-mode-common-hook 'visual-line-mode)
  (add-hook 'c-mode-common-hook 'adaptive-wrap-prefix-mode)
  (add-hook 'c-mode-common-hook (lambda ()
                                  (setq adaptive-wrap-extra-indent 4)))
  (add-hook 'c-mode-common-hook 'init-c-flymake)
  (add-hook 'c-mode-common-hook 'init-c-autocomplete))

(set-c-options)

(provide 'c-config)
