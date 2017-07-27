(defun current-file-is-c++-p ()
  (and (string= (file-name-extension buffer-file-name) "h")
       (or (re-search-forward "[^@]class"
                              magic-mode-regexp-match-limit
                              t)
           (re-search-forward "namespace"
                              magic-mode-regexp-match-limit
                              t)
           (re-search-forward "virtual"
                              magic-mode-regexp-match-limit
                              t)
           (re-search-forward "private:"
                              magic-mode-regexp-match-limit
                              t)
           (re-search-forward "public:"
                              magic-mode-regexp-match-limit
                              t))))

(use-package c-config
  :no-require
  :magic (current-file-is-c++-p . c++-mode))

(provide 'c++-config)
