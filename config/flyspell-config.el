(defun set-flyspell-options ()

  ;; Mouse 3 (right click) is for spellchecking, not mouse 2 (middle)!
  (eval-after-load "flyspell"
    '(define-key flyspell-mode-map [down-mouse-3] 'flyspell-correct-word))
  
  ;; Start up flyspell mode when it makes sense
  (add-hook 'text-mode-hook 'flyspell-mode))

(set-flyspell-options)

(provide 'flyspell-config)
