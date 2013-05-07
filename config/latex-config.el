(defun set-latex-options ()
  (require-package 'auctex)

  (setq LaTeX-command "pdflatex")

  (add-hook 'LaTeX-mode-hook 'visual-line-mode)

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
  )

(set-latex-options)

(provide 'latex-config)
