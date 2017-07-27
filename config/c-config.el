(use-package cc-mode
  :ensure nil
  :magic (((rx (sequence line-start "#include <" (+ alpha) ">" line-end)) . c++-mode)
          ((rx (sequence line-start "#import <Foundation/Foundation.h>")) . objc-mode))
  :config
  (progn
    (setq c-default-style
          (quote
           ((c-mode . "stroustrup")
            (c++-mode . "stroustrup")
            (java-mode . "java")
            (awk-mode . "awk")
            (other . "gnu"))))
    (use-package find-file
      :config
      (progn
        (nconc (cadr (assoc "\\.h\\'" cc-other-file-alist)) '(".mm"))
        (add-to-list 'cc-other-file-alist '("\\.mm\\'" (".h")))))))

(use-package flyspell-config
  :ensure nil
  :after cc-mode
  :config
  (add-hook 'c-mode-common-hook 'flyspell-prog-mode))

(provide 'c-config)
