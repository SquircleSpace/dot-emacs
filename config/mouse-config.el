(defun smooth-scroll (number-lines increment)
  (if (= 0 number-lines)
      t
    (progn
      (sit-for 0.02)
      (scroll-up increment)
      (smooth-scroll (- number-lines 1) increment))))

(defun set-smooth-scroll ()
  ;; The scrolling experience is good enough
  (unless (fboundp 'mac-mwheel-scroll)
    (if (window-system)
        (progn
          (global-set-key (kbd "<wheel-down>")
                          (lambda () (interactive) (smooth-scroll 3 1)))
          (global-set-key (kbd "<wheel-up>")
                          (lambda () (interactive) (smooth-scroll 3 -1)))))
    (progn
      (global-set-key [(mouse-5)]
                      (lambda () (interactive) (smooth-scroll 3 1)))
      (global-set-key [(mouse-4)]
                      (lambda () (interactive) (smooth-scroll 3 -1))))))

(defun set-text-mode-mouse-options ()
  ;; Enable mouse
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t)

  (set-smooth-scroll))

(defun set-window-system-mouse-options ()
  (setq mouse-wheel-scroll-amount '(1))
  (setq mouse-wheel-progressive-speed nil))

(if (window-system)
    (set-window-system-mouse-options)
  (set-text-mode-mouse-options))

(provide 'mouse-config)
