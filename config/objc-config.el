(require-package 'anything)
(require 'anything)
(require 'anything-config)

(defvar anything-c-source-objc-headline
  '((name . "Objective-C Headline")
    (headline . "^[ \t]*[-+@]\\|^#pragma[ \t]+mark")))

(defun objc-headline ()
  (interactive)
  (let ((anything-candidate-number-limit 500))
    (anything-other-buffer '(anything-c-source-objc-headline)
                           "*ObjC Headline")))

(defun set-objc-options ()
  (require 'autocomplete-config)
  (require 'flymake-config)
  (require-package 'adaptive-wrap)

  (add-to-list 'load-path "~/.emacs.d/elisp/")
  (require 'docsetutil)
  (require 'w3m)
  (setq docsetutil-program
        "/Applications/Xcode.app/Contents/Developer/usr/bin/docsetutil")
  (setq docsetutil-browse-url-function 'w3m-browse-url)

  (add-hook 'objc-mode-hook 'visual-line-mode)
  (add-hook 'objc-mode-hook 'adaptive-wrap-prefix-mode)
  (add-hook 'objc-mode-hook (lambda ()
                              (setq adaptive-wrap-extra-indent 4)))
  (add-to-list 'ac-modes 'objc-mode)

  ;; Add keyboard shortcuts
  (add-hook 'objc-mode-hook
            (lambda ()
              (define-key objc-mode-map (kbd "C-c d") 'docsetutil-search)
              (define-key objc-mode-map (kbd "C-c o") 'ff-find-other-file)
              (define-key objc-mode-map (kbd "C-c h") 'objc-headline)))

  ;; Support .mm files
  (add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

  ;; Support quickly switching between header and implementation
  (require 'find-file)
  (nconc (cadr (assoc "\\.h\\'" cc-other-file-alist)) '(".m" ".mm"))
  (add-to-list 'cc-other-file-alist '("\\.m\\'" (".h")))
  (add-to-list 'cc-other-file-alist '("\\.mm\\'" (".h")))

  ;; Try to detect objecitve c headers
  (add-to-list 'magic-mode-alist
               `(,(lambda ()
                    (and (string= (file-name-extension buffer-file-name) "h")
                         (or (re-search-forward "@\\<interface\\>"
                                                magic-mode-regexp-match-limit
                                                t)
                             (re-search-forward "@\\<protocol\\>"
                                                magic-mode-regexp-match-limit
                                                t))))
                 . objc-mode))
  )

(set-objc-options)

(provide 'objc-config)
