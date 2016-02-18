;; Inspired by Emacs Prelude
(require-package 'ido 'ido-ubiquitous 'ido-yes-or-no 'flx-ido)
(require 'ido)
(require 'ido-ubiquitous)
(require 'ido-yes-or-no)
(require 'flx-ido)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-use-filename-at-point nil
      ido-max-prospects 10
      ido-save-directory-list-file (expand-file-name "ido.hist"
                                                     user-emacs-directory)
      ido-default-file-method 'selected-window)

;; disable ido faces to see flx highlights
;(setq ido-use-faces nil)

;; Turn on ido
(ido-mode)

;; smarter fuzzy matching for ido
(flx-ido-mode +1)

;; More ido is more better
(ido-everywhere)
(ido-ubiquitous-mode)
(ido-yes-or-no-mode)

;; Make ido not do that annoying "search for recent files matching
;; what you typed" thing.
(setq ido-auto-merge-work-directories-length -1)

(provide 'ido-config)
