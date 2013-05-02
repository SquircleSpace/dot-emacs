(defun set-mouse-options ()
  ; gui emacs already has mouse support
  (unless window-system
    ;; Enable mouse
    (require 'mouse)
    (xterm-mouse-mode t)
    (defun track-mouse (e))
    (setq mouse-sel-mode t)

    ;; Mouse scrolling
    (defun smooth-scroll (number-lines increment)
      (if (= 0 number-lines)
          t
        (progn
          (sit-for 0.02)
          (scroll-up increment)
          (smooth-scroll (- number-lines 1) increment))))

    (global-set-key [(mouse-5)] '(lambda () (interactive) (smooth-scroll 3 1)))
    (global-set-key [(mouse-4)] '(lambda () (interactive) (smooth-scroll 3 -1)))
    )
  )

(set-mouse-options)

(provide 'mouse-config)
