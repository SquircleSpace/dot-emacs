(defun current-file-is-objc-p ()
  (and (string= (file-name-extension buffer-file-name) "h")
       (or (re-search-forward "@\\<interface\\>"
                              magic-mode-regexp-match-limit
                              t)
           (re-search-forward "@\\<protocol\\>"
                              magic-mode-regexp-match-limit
                              t)
           (re-search-forward "@\\<class\\>"
                              magic-mode-regexp-match-limit
                              t))))

(use-package c-config
  :magic ((current-file-is-objc-p . objc-mode)))

(use-package find-file
  :no-require t
  :config
  (progn
    (nconc (cadr (assoc "\\.h\\'" cc-other-file-alist)) '(".mm"))
    (add-to-list 'cc-other-file-alist '("\\.mm\\'" (".h")))))

(provide 'objc-config)
