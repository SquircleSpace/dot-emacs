(defun set-shell-options ()
  ;; Make ehsell have case insensitive tab complete
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq pcomplete-ignore-case t)))

  ;; Make shell output read only
  (add-hook
   'comint-output-filter-functions
   (lambda (string)
     ;; Need to inhibit read only to re-read-onlyify everything
     (let ((inhibit-read-only t))
       (add-text-properties (point-min) (point-max)
                            '(read-only t front-sticky (read-only))))))

  ; Use colors
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

  ; Make shell mode use zsh
  (setq explicit-shell-file-name "/bin/zsh")

  ; Assume shell echo commands you send them. Don't echo for them.
  (setq comint-process-echoes t)

  ; Don't allow changing shell prompt
  (setq comint-prompt-read-only t)

  ; Input always goes to the prompt
  (setq comint-scroll-to-bottom-on-input 'this)
  (setq eshell-scroll-to-bottom-on-input 'this)

  ; No welcome message when launching eshell
  (setq eshell-banner-message "")
  )

(set-shell-options)

(provide 'shell-config)
