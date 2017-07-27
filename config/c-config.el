(use-package cc-mode
  :no-require t
  :after
  (setq c-default-style
        (quote
         ((c-mode . "stroustrup")
          (c++-mode . "stroustrup")
          (java-mode . "java")
          (awk-mode . "awk")
          (other . "gnu")))))

(use-package flyspell-config
  :no-require t
  :after cc-mode
  :config
  (add-hook 'c-mode-common-hook 'flyspell-prog-mode))

(provide 'c-config)
