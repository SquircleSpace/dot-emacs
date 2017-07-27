(use-package flyspell-config)

(use-package org
  :mode (("\\.org$" . org-mode))
  :config
  (progn
    ;; visual-line and org-indent
    (add-hook 'org-mode-hook 'visual-line-mode)
    (add-hook 'org-mode-hook 'org-indent-mode)

    ;; Flyspell
    (add-hook 'org-mode-hook 'flyspell-mode)))

(provide 'org-config)
