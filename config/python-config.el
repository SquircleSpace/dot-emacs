(defun set-python-options ()
  (require 'flyspell-config)
  (add-hook 'python-mode-hook 'flyspell-prog-mode))

(set-python-options)

(provide 'python-config)
