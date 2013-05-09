(defun set-fringe-options ()
  ;; Get a very small right fringe
  (set-fringe-mode '(0 . 1))
  ;; Put useful things in it
  (setq-default indicate-buffer-boundaries 'right)
  (setq-default indicate-empty-lines t)
  ;; We don't need to indicate empty lines in whitespace mode anymore
  (setq whitespace-style (delete 'empty whitespace-style))

  ;; By default, scrolling in fringes and margins is unbound. This is
  ;; silly. Make it do the sane thing and scroll like normal.
  (dolist (prefix '("" "double-" "triple-"))
    (dolist (direction '("up" "down" "left" "right"))
      (dolist (side '("left" "right"))
        (dolist (type '("fringe" "margin"))
          (let* ((fringe-str (concat "<" side "-" type ">"))
                 (wheel-str (concat "<" prefix "wheel-" direction ">"))
                 (from-kbd (kbd (concat fringe-str " " wheel-str)))
                 (to-fn (global-key-binding (kbd wheel-str))))
            (global-set-key from-kbd to-fn))))))
  )

;; Can't set fringe options without a window system
(when window-system
  (set-fringe-options))

(provide 'fringe-config)
