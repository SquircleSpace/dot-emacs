(defun set-c-options ()

  (setq c-default-style
        (quote
         ((c-mode . "stroustrup")
          (c++-mode . "stroustrup")
          (java-mode . "java")
          (awk-mode . "awk")
          (other . "gnu"))))

  (require-package 'adaptive-wrap)

  (add-hook 'c-mode-common-hook 'visual-line-mode)
  (add-hook 'c-mode-common-hook 'adaptive-wrap-prefix-mode)
  (add-hook 'c-mode-common-hook (lambda ()
                                  (setq adaptive-wrap-extra-indent 4))))

(set-c-options)

(provide 'c-config)
