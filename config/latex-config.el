(use-package tex
  :ensure auctex
  :config
  (progn
    (setf LaTeX-command "pdflatex")
    (add-hook 'LaTeX-mode-hook 'visual-line-mode)
    (add-hook 'LaTeX-mode-hook 'adaptive-wrap-prefix-mode)))

(use-package tex
  :if (eq system-type 'darwin)
  :config
  ;; Make auctex do the right thing with open.
  (setf TeX-view-program-list
        '(("Open" "open %s.pdf")))
  (setf TeX-view-program-selection
        '(((output-dvi style-pstricks) "dvips and gv")
          (output-dvi "Open")
          (output-pdf "Open")
          (output-html "Open"))))

(use-package flyspell-config
  :after tex
  :config
  (add-hook 'LaTeX-mode-hook 'flyspell-mode))

(provide 'latex-config)
