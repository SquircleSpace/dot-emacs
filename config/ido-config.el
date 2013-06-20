(defun set-ido-options ()
  (require-package 'ido 'ido-ubiquitous 'ido-yes-or-no)
  (require 'ido)
  (require 'ido-ubiquitous)
  (require 'ido-yes-or-no)

  ;; Turn on ido
  (ido-mode)

  ;; Use flex matching
  (setq ido-enable-flex-matching t)

  ;; More ido is more better
  (ido-everywhere)
  (ido-ubiquitous-mode)
  (ido-yes-or-no-mode)

  ;; Make ido not do that annoying "search for recent files matching
  ;; what you typed" thing.
  (setq ido-auto-merge-work-directories-length -1))

(set-ido-options)

(provide 'ido-config)
