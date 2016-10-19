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
    (require 'autocomplete-config)

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
  ;; clang-async seems to hate being used with any other completion source
  (setq ac-sources '(ac-source-clang-async))

  ;; Need to explicitly launch process
  (unless ac-clang-completion-process
    (ac-clang-launch-completion-process)))


(setq c-default-style
      (quote
       ((c-mode . "stroustrup")
        (c++-mode . "stroustrup")
        (java-mode . "java")
        (awk-mode . "awk")
        (other . "gnu"))))

(require 'flymake-config)
(add-hook 'c-mode-common-hook 'init-c-flymake)

;; Set up autocomplete, but override default ac-cc-mode-setup.
(require 'autocomplete-config)
(defun ac-cc-mode-setup ())

(add-hook 'c-mode-common-hook 'init-c-autocomplete)

(require 'flyspell-config)
(add-hook 'c-mode-common-hook 'flyspell-prog-mode)

(provide 'c-config)
