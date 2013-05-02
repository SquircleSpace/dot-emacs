(defun set-cs70-options ()
  ;; Add CS70 Rubric support
  (load "~/.emacs.d/elisp/rubric.el")
  (add-to-list 'auto-mode-alist '("Rubric\.\*\\.txt" . rubric-mode))
  )

(set-cs70-options)

(provide 'grader-config)
