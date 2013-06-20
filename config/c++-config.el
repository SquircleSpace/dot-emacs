(defun set-c++-options ()
  (require 'c-config)
  
  (add-to-list 'magic-mode-alist
               `(,(lambda ()
                    (and (string= (file-name-extension buffer-file-name) "h")
                         (or (re-search-forward "class"
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
                 . c++-mode)))

(set-c++-options)

(provide 'c++-config)
