(require-package 'anything)
(require 'anything)

(defun get-objc-selector-raw ()
  (let ((ignore-set '("@"))
        (skip-set '("(" "{" "[" "\"" "'"))
        (accum "")
        (stay t)
        (char (lambda ()
                (buffer-substring-no-properties
                 (point) (+ 1 (point))))))
    (while stay
      (cond ((member (funcall char) skip-set)
             (progn
               (forward-sexp)))
            ((member (funcall char) ignore-set)
             (progn
               (forward-char)))
            ((string-equal (funcall char) "]")
             (progn
               (setq stay nil)))
            (t
             (progn
               (setq accum (concat accum (funcall char)))
               (forward-char)))))
    accum))

(defun get-objc-selector ()
  (save-excursion
    ;; Step just inside the call
    (up-list)
    (backward-list)
    (down-list)

    (let* ((raw (get-objc-selector-raw))
           (has-args (string-match ":" raw))
           (cleaned "")
           (args-regex "\\([a-zA-Z]+\\)[ \t\n]*:")
           (noargs-regex "[a-zA-Z]+")
           (start-point 0))
      (cond (has-args
             (while (string-match args-regex raw start-point)
               (setq cleaned (concat cleaned (match-string 1 raw) ":"))
               (setq start-point (match-end 0))))
            (t
             (while (string-match noargs-regex raw start-point)
               (setq start-point (match-end 0))
               (setq cleaned (match-string 0 raw)))))
      cleaned)))

(defun search-objc-function ()
  (interactive)
  (let ((name (get-objc-selector)))
    (docsetutil-search name)))

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
              (define-key objc-mode-map (kbd "C-c f") 'search-objc-function)
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
