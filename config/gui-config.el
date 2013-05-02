(defun set-gui-options ()
  ;; Bar cursor
  (set-default 'cursor-type 'bar)

  ;; Show Column numbers
  (setq column-number-mode t)

  ;; No menu bar
  (menu-bar-mode -1)

  ;; Set gui options
  (when window-system

    ;; Default frame size
    (setq default-frame-alist
          '((width . 80)
            (height . 40)))

    ;; Default buffer
    (setq inhibit-startup-screen t)
    (setq initial-buffer-choice nil)
    
    (set-fringe-mode 0)

    (set-scroll-bar-mode nil)

    (tool-bar-mode 0)

    )
  )

(set-gui-options)

(provide 'gui-config)
