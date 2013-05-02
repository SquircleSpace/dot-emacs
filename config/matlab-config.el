(defun set-matlab-options ()
  ;; Add matlab support
  (add-to-list 'load-path "~/.emacs.d/elisp/mlab")
  (load-library "matlab-load")
  )

(set-matlab-options)

(provide 'matlab-config)
