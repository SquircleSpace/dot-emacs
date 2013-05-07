(defvar needed-packages
  (list)
  "The list of packages that are required."
  )

(defvar package-refreshed
  nil
  "Whether we have refreshed the package listing in this execution.")

(defun require-package (&rest rest-var)
  "Indicate that a package is required."
  
  (dolist (p rest-var)
    (if (not (package-installed-p p))
        (progn
          (unless package-refreshed
            (package-refresh-contents)
            (setq package-refreshed t))
          (package-install p)
          (setq needed-packages (cons p needed-packages)))
      (progn
        (add-to-list 'needed-packages p))))
  )

(defun install-packages ()
  (unless package-refreshed
    (package-refresh-contents)
    (setq package-refreshed t)
    )
  (dolist (p needed-packages)
    (when (not (package-installed-p p))
      (package-install p)))
  )

(defun all-packages-installed ()
  (require 'cl)
  (loop for p in needed-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

(defun set-package-options ()
  ;; Init package
  (require 'package)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)

  (when (not (all-packages-installed))
    (install-packages))

  ;; What does this do?
  ;(package-menu--find-upgrades)

  )

(set-package-options)

(provide 'package-config)
