(defun rgrep-fast (regexp &optional files dir confirm)
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((and grep-find-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " grep-find-command
                                   nil nil 'grep-find-history)))
      ((not grep-find-template)
       (error "grep.el: No `grep-find-template' available"))
      (t (let* ((regexp (grep-read-regexp))
                (files "*")
                (dir (if (magit-toplevel)
                         (magit-toplevel)
                       (read-directory-name "Base directory: "
                                            nil default-directory t)))
                (confirm (equal current-prefix-arg '(4))))
           (list regexp files dir confirm))))))
  (let ((grep-find-ignored-files grep-find-ignored-files)
        (grep-find-template (if (magit-toplevel dir)
                                "git --no-pager grep -nH <C> -e <R>"
                              grep-find-template)))
    (rgrep regexp files dir confirm)))

(global-set-key (kbd "M-]") 'rgrep-fast)

(provide 'grep-config)
