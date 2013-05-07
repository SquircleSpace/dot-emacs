;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

(defun set-misc-options ()

  ;; Don't polute everything with backups
  (setq backup-directory-alist
        `(("." . ,(concat user-emacs-directory "backups/"))))

  ;; No bell
  ; Do nothing
  (setq ring-bell-function (lambda () (progn)))
  ; Flash
  ;(setq visible-bell t)

  ;; Always add new line
  (setq require-final-newline t)

  ;; No tab characters
  (setq-default indent-tabs-mode nil)
  (setq tab-width 4)

  ;; Delete selected text
  (pending-delete-mode 1)

  ;; Overwrite selected text
  (delete-selection-mode 1)

  ;; Don't jump when cursor goes out of screen
  (setq scroll-conservatively 10000)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Apply options

(if (< emacs-major-version 24)
    (progn
      (error "This version of emacs is as old as dirt."))
  )

(add-to-list 'load-path "~/.emacs.d/config/")

(require 'package-config)

(set-misc-options)

(require 'grader-config)
(require 'matlab-config)
(require 'shell-config)
(require 'org-config)
(require 'latex-config)
(require 'python-config)
(require 'haskell-config)
(require 'c++-config)

(require 'whitespace-config)
(require 'autocomplete-config)
(require 'flyspell-config)
(require 'flymake-config)
(require 'ido-config)

(require 'magit-config)

(require 'mouse-config)

(require 'gui-config)
(require 'system-config) ; must be last
