(package-initialize)

(when (< emacs-major-version 25)
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
(if (file-exists-p "~/.emacs.d/config/machine-config.el")
    (require 'machine-config))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
