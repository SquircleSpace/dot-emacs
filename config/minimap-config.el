(require-package 'minimap)

;; Minimap is sometimes-want feature. Only bother to do the settings
;; if we want minimap!
(eval-after-load 'minimap
  ;; Don't let other things steal the window
  '(setq minimap-dedicated-window t))

(provide 'minimap-config)
