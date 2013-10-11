;; OS-specific changes
(when (eq system-type 'darwin)
  ;; keyboard shortcuts
  (setq mac-command-modifier (quote super))
  (setq mac-option-modifier (quote meta))
  (global-set-key [(super a)] 'mark-whole-buffer)
  (global-set-key [(super v)] 'yank)
  (global-set-key [(super c)] 'kill-ring-save)
  (global-set-key [(super x)] 'kill-region)
  (global-set-key [(super s)] 'save-buffer)
  (global-set-key [(super f)] 'search-forward)
  (global-set-key [(super l)] 'goto-line)
  (global-set-key [(super q)] 'save-buffers-kill-terminal)
  (global-set-key [(super w)]
                  (lambda () (interactive) (delete-window)))
  (global-set-key [(super z)] 'undo)

  (when window-system
    ;; We are running a guifull emacs! Fix our path.
    (require-package 'exec-path-from-shell)
    (let ((old-exec exec-path))
      (exec-path-from-shell-initialize)
      (setq exec-path (append exec-path old-exec)))))

(defun set-machine-options-post-init ()
  ;; OS-specific changes
  (when (and (eq system-type 'darwin)
             window-system)
    ;; Enable menubar again (full screen button needs it?!?)
    (menu-bar-mode 1))

  ;; Check if the extra elisp file exists
  (if (file-exists-p "~/.emacs.d/machine.el")
      (load "~/.emacs.d/machine.el")))

(add-hook 'after-init-hook 'set-machine-options-post-init)

(provide 'system-config)
