(defun set-misc-options ()

  ;; Don't polute everything with backups
  (setq backup-directory-alist
        `(("." . ,(concat user-emacs-directory "backups/"))))

  ;; Always add new line
  (setq require-final-newline t)

  ;; No tab characters
  (setq-default indent-tabs-mode nil)

  ;; Fix shift up
  (defadvice terminal-init-xterm (after select-shift-up activate)
    (define-key input-decode-map "\e[1;2A" [S-up]))

  ;; Use SSH when tramping.
  (setq tramp-default-method "ssh")

  ;; Make scrolling in docview mode continuous
  (setq doc-view-continuous t)

  ;; Support unfill
  (require-package 'unfill)

  ;; Undo tree
  (require-package 'undo-tree)
  )

;; Apply options

(when (< emacs-major-version 24)
  (error "This version of emacs is as old as dirt."))

(add-to-list 'load-path "~/.emacs.d/config/")

(require 'package-config)
(require 'system-config) ; Should come early

(set-misc-options)

(require 'grader-config)
(require 'shell-config)
(require 'org-config)
(require 'latex-config)
(require 'python-config)
(require 'haskell-config)
(require 'c++-config)
(require 'objc-config)

(require 'autocomplete-config)
(require 'flyspell-config)
(require 'flymake-config)
(require 'ido-config)

(require 'magit-config)

(require 'mouse-config)

(require 'ui-config)
(require 'fringe-config)
