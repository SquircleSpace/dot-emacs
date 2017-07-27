(defun readonlyify-comint-output (string)
  ;; Need to inhibit read only to re-read-onlyify everything
  (let ((inhibit-read-only t))
    (add-text-properties (point-min) (point-max)
                         '(read-only t front-sticky (read-only)))))

(use-package comint
  :ensure nil
  :config
  (progn
    ;; Make shell output read only
    (add-hook 'comint-output-filter-functions
              'readonlyify-comint-output)

    ;; Input always goes to the prompt
    (setf comint-scroll-to-bottom-on-input 'this)

    ;; Don't allow changing shell prompt
    (setf comint-prompt-read-only t)))

(use-package shell
  :ensure nil
  :config
  (progn
    ;; Use colors
    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)))

(use-package eshell
  :ensure nil
  :config
  (progn
    ;; Input always goes to the prompt
    (setf eshell-scroll-to-bottom-on-input 'this)

    ;; No welcome message when launching eshell
    (setq eshell-banner-message "")))

(provide 'shell-config)
