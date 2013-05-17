(defun set-objc-options ()
  (require-package 'anything)
  (require 'anything)

  (defun get-objc-selector-raw ()
    "Get the characters that are direct residents of the selector's braces.
The expression [[foo bar] a:@\"stuff\" b:[baz zap] c:wow] will
return \" a: b: c:wow\". This string can be further processed to
get just the selector."
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
    "Try to extract a selector name from the area around point.
If the text around point is [[foo bar] a:@\"stuff\" b:[baz zap]
c:wow] then this will return \"a:b:c:\"."
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

  (defun search-objc-selector ()
    "Try to search the documentation for information about the
selector near point."
    (interactive)
    (let ((name (get-objc-selector)))
      (docsetutil-search name)))

  (defun search-objc-name ()
    "Search the documentation for the word under point."
    (interactive)
    (docsetutil-search (thing-at-point 'symbol))
    )

  (defvar anything-c-source-objc-headline
    '((name . "Objective-C Headline")
      (headline . "^[ \t]*[-+@]\\|^#pragma[ \t]+mark")))

  (defun objc-headline ()
    "Produce a list of all selectors and #pragma marks defined in buffer."
    (interactive)
    (let ((anything-candidate-number-limit 500))
      (anything-other-buffer '(anything-c-source-objc-headline)
                             "*ObjC Headline")))

  (defun objc-xcode-build ()
    "Attempt to build the project."
    (interactive)
    (compile (concat "xcodebuild -configuration Debug -project "
                     (find-xcode-proj))))

  (defun objc-xcode-flymake-filter (process output)
    "Process output from xcodebuild.
Mostly copied from flymake.el."
    (let ((source-buffer (process-buffer process)))
      (flymake-log 3 "received %d byte(s) of output from process %d"
                   (length output) (process-id process))
      (when (buffer-live-p source-buffer)
        (with-current-buffer source-buffer
            (flymake-parse-output-and-residual output)))))

  (defun objc-xcode-flymake-sentinel (process _event)
    "Handle termination of xcodebuild.
Mostly copied from flymake.el. Removed check for non-zero exit
status and no errors."
    (when (memq (process-status process) '(signal exit))
      (flymake-parse-residual)
      (setq flymake-err-info flymake-new-err-info)
      (setq flymake-new-err-info nil)
      (setq flymake-err-info
            (flymake-fix-line-numbers
             flymake-err-info 1 (flymake-count-lines)))
      (flymake-delete-own-overlays)
      (flymake-highlight-err-lines flymake-err-info)
      (let (err-count warn-count (exit-status (process-exit-status process)))
        (setq err-count (flymake-get-err-count flymake-err-info "e"))
        (setq warn-count  (flymake-get-err-count flymake-err-info "w"))
        (flymake-log 2 "%s: %d error(s), %d warning(s)"
                     (buffer-name) err-count warn-count)
        (setq flymake-check-start-time nil)

        (if (and (equal 0 err-count) (equal 0 warn-count))
            (if (equal 0 exit-status)
                (flymake-report-status "" "")	; PASSED
              (flymake-report-status nil ""))) ; "STOPPED"
        (flymake-report-status (format "%d/%d" err-count warn-count) ""))))


  (defun objc-xcode-flymake-init ()
    "Produce command for checking syntax."
    (let ((process
           (apply 'start-file-process "flymake-proc" (current-buffer)
                  "xcodebuild" `("-configuration" "Debug"
                                 "-project" ,(find-xcode-proj)))))
      (set-process-filter process 'objc-xcode-flymake-filter)
      (set-process-sentinel process 'objc-xcode-flymake-sentinel)
      nil))

  (defun find-xcode-proj ()
    "Attempt to locate the xcodeproj directory."
    (if (directory-files "." t ".*\.xcodeproj$" nil)
        (nth 0 (directory-files "." t ".*\.xcodeproj$" nil))
      (let ((old-dir default-directory))
        (cd "../")
        (let ((result (find-xcode-proj)))
          (cd old-dir)
          result))))

  (require 'autocomplete-config)
  (require 'flymake-config)
  (require-package 'adaptive-wrap)
  (require-package 'w3m)

  (add-to-list 'load-path "~/.emacs.d/elisp/")
  (require 'docsetutil)
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
              (define-key objc-mode-map (kbd "C-c m") 'search-objc-selector)
              (define-key objc-mode-map (kbd "C-c f") 'search-objc-name)
              (define-key objc-mode-map (kbd "C-c d") 'docsetutil-search)
              (define-key objc-mode-map (kbd "C-c o") 'ff-find-other-file)
              (define-key objc-mode-map (kbd "C-c b") 'objc-xcode-build)
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

  ;; Provide flymake support for Objective-C implementation files
  (push '("\\.mm?\\'" objc-xcode-flymake-init) flymake-allowed-file-name-masks)
  )

;; objc takes a while to set up. Only do it if we need to.
(eval-after-load 'cc-mode
  '(set-objc-options))

(provide 'objc-config)
