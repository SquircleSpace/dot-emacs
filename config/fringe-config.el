(when window-system
  ;; Get a very small right fringe
  (set-fringe-mode '(5 . 5))
  ;; Put useful things in it
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default indicate-empty-lines t)
  (setq flymake-fringe-indicator-position 'left-fringe)
  (setq visual-line-fringe-indicators '(nil right-curly-arrow))

  ;; By default, scrolling in fringes and margins is unbound. This is
  ;; silly. Make it do the sane thing and scroll like normal. Be
  ;; careful, though! At least one Emacs mac port rebinds the scroll
  ;; keys after init! So, we go through a layer of indirection and
  ;; lookup the correct function to call every time the scroll event
  ;; happens.
  (dolist (direction '("up" "down" "left" "right"))
    (dolist (side '("left" "right"))
      (dolist (type '("fringe" "margin"))
        (let* ((fringe-str (concat "<" side "-" type ">"))
               (wheel-str (concat "<wheel-" direction ">"))
               (from-kbd (kbd (concat fringe-str " " wheel-str)))
               (to-fn `(lambda (event)
                         ; Need to be interactive, for some
                         ; reason. Copied paramater from function we
                         ; are wraping.
                         (interactive (list last-input-event))
                         ; Call the real function, but bake the key to
                         ; lookup into the lambda function
                         (call-interactively (global-key-binding
                                              ,(kbd wheel-str))
                                             nil [event]))))
          (global-set-key from-kbd (eval to-fn)))))))

(provide 'fringe-config)
