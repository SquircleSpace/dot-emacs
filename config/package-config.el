;; Bootstrap package installation
(add-to-list 'package-selected-packages 'use-package)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setf use-package-always-ensure t)

;; Init package
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(provide 'package-config)
