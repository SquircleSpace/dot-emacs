(require-package 'org)

;; visual-line and org-indent
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'org-indent-mode)

;; Flyspell
(require 'flyspell-config)
(add-hook 'org-mode-hook 'flyspell-mode)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(provide 'org-config)
