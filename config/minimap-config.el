(defun set-minimap-options ()
  (require-package 'minimap)

  ;; Don't let other things steal the window
  (eval-after-load 'minimap
    '(setq minimap-dedicated-window t))
  )

(set-minmap-options)

(provide 'minimap-config)
