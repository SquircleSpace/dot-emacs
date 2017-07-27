(use-package flyspell
  :ensure nil
  :init
  ;; Start up flyspell mode when it makes sense
  (add-hook 'text-mode-hook 'flyspell-mode)
  :config
  ;; Mouse 3 (right click) is for spellchecking, not mouse 2 (middle)!
  (define-key flyspell-mode-map [down-mouse-3] 'flyspell-correct-word))

(provide 'flyspell-config)
