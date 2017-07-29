(use-package autocomplete-config
  :ensure nil)

(use-package ac-slime)

(use-package slime
  :after lisp-mode
  :config
  (progn
    (setf slime-contribs '(slime-fancy))

    (add-hook 'slime-mode-hook 'set-up-slime-ac)
    (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
    (use-package ac-slime
      :config
      (add-to-list 'ac-modes 'slime-repl-mode))))

(provide 'lisp-config)
