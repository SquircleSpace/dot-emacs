(use-package c-config
  :magic (((rx (sequence line-start "#import <Foundation/Foundation.h>")) . objc-mode)))

(use-package find-file
  :no-require t
  :config
  (progn
    (nconc (cadr (assoc "\\.h\\'" cc-other-file-alist)) '(".mm"))
    (add-to-list 'cc-other-file-alist '("\\.mm\\'" (".h")))))

(provide 'objc-config)
