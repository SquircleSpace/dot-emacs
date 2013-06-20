(defun set-latex-options ()
  (require-package 'auctex)
  (require-package 'adaptive-wrap)

  (setq LaTeX-command "pdflatex")

  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'adaptive-wrap-prefix-mode)

  (when (eq system-type 'darwin)
    ;; Make auctex do the right thing with open.
    (setq TeX-view-program-list
          (quote (("Open" "open %s.pdf"))))
    (setq TeX-view-program-selection
          (quote (
                  ((output-dvi style-pstricks) "dvips and gv")
                  (output-dvi "Open")
                  (output-pdf "Open")
                  (output-html "Open")))))

  (require 'flyspell-config)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode))

(set-latex-options)

(provide 'latex-config)
