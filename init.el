(when (< emacs-major-version 24)
  (error "This version of emacs is as old as dirt."))

(add-to-list 'load-path "~/.emacs.d/config/")

(require 'package-config)
(require 'system-config) ; Should come early
(require 'misc-config)

(require 'flyspell-config)
(require 'ido-config)
(require 'magit-config)
(require 'mouse-config)

(require 'ui-config)
(require 'fringe-config)
(require 'grep-config)

;; Check if the extra elisp file exists
(if (file-exists-p "~/.emacs.d/machine.el")
    (load "~/.emacs.d/machine.el"))
