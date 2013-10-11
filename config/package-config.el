(defvar package-refreshed
  nil
  "Whether we have refreshed the package listing in this execution.")

(defun require-package (&rest rest-var)
  "Indicate that a package is required."

  (dolist (p rest-var)
    (when (not (package-installed-p p))
      (unless package-refreshed
        (package-refresh-contents)
        (setq package-refreshed t))
      (package-install p)))
  )

;; Init package
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(provide 'package-config)
