(use-package whitespace
  :config
  (progn
    (setf whitespace-style
          '(trailing
            empty
            space-after-tab
            space-before-tab
            lines-tail
            indentation
            face))))

(provide 'whitespace-config)
