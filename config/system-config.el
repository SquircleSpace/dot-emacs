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
  (global-set-key [(super z)] 'undo))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  ;; We are running a guifull emacs! Fix our path.
  :demand t
  :config
  (exec-path-from-shell-initialize))

(provide 'system-config)
