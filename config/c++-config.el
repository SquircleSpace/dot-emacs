(defun set-c++-options ()
  ;; Set C++ style
  (setq c-default-style
    (quote
     ((c-mode . "stroustrup")
      (c++-mode . "stroustrup")
      (java-mode . "java")
      (awk-mode . "awk")
      (other . "gnu"))))
  )

(provide 'c++-config)
