;; Inspired by Emacs Prelude
(use-package flx-ido)
(use-package ido-yes-or-no)

(use-package ido
  :ensure nil
  :demand t
  :config
  (progn
    (setf ido-enable-prefix nil)
    (setf ido-enable-flex-matching t)
    (setf ido-use-filename-at-point nil)
    (setf ido-max-prospects 10)
    (setf ido-save-directory-list-file
          (expand-file-name "ido.hist" user-emacs-directory))
    (setf ido-default-file-method 'selected-window)
    (ido-mode 1)

    ;; smarter fuzzy matching for ido
    (flx-ido-mode 1)

    ;; More ido is more better
    (ido-everywhere)
    (ido-yes-or-no-mode 1)

    ;; Make ido not do that annoying "search for recent files matching
    ;; what you typed" thing.
    (setf ido-auto-merge-work-directories-length -1)))

(provide 'ido-config)
