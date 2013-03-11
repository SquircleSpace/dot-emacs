;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global

(defun set-global-options ()

  ;; Bar cursor
  (set-default 'cursor-type 'bar)

  ;; Make shell mode use zsh
  (setq explicit-shell-file-name "/bin/zsh")

  ;; Use word-count mode
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/"))
  (require 'wc-mode)

  ;; Show Column numbers
  (setq column-number-mode t)

  ;; Always add new line
  (setq require-final-newline t)

  ;; No tab characters
  (setq-default indent-tabs-mode nil)
  (setq tab-width 4)

  ;; Delete selected text
  (pending-delete-mode 1)

  ;; Overwrite selected text
  (delete-selection-mode 1)

  ;; Don't jump when cursor goes out of screen
  (setq scroll-conservatively 10000)

  ;; Use hunspell for spellchecking
  (when (executable-find "hunspell")
    (setq-default ispell-program-name "hunspell")
    (setq ispell-dictionary "american"
          ispell-extra-args '() ;; TeX mode "-t"
          ispell-silently-savep t
          )

    (add-hook 'ispell-initialize-spellchecker-hook
              (lambda ()
                (setq ispell-base-dicts-override-alist
                      '((nil ; default
                         "[A-Za-z]" "[^A-Za-z]" "[']" t
                         ("-d" "en_US" "-i" "utf-8") nil utf-8)
                        ("american" ; Yankee English
                         "[A-Za-z]" "[^A-Za-z]" "[']" t
                         ("-d" "en_US" "-i" "utf-8") nil utf-8)
                        ("british" ; British English
                         "[A-Za-z]" "[^A-Za-z]" "[']" t
                         ("-d" "en_GB" "-i" "utf-8") nil utf-8)))))
    )

  ;; Fix shift up
  (defadvice terminal-init-xterm (after select-shift-up activate)
    (define-key input-decode-map "\e[1;2A" [S-up]))

  ;; Set terminal emacs options
  (unless window-system
    ;; Enable mouse support
    (require 'mouse)
    (xterm-mouse-mode t)
    (defun track-mouse (e))
    (setq mouse-sel-mode t)

    ;; Mouse scrolling
    (defun smooth-scroll (number-lines increment)
      (if (= 0 number-lines)
          t
        (progn
          (sit-for 0.02)
          (scroll-up increment)
          (smooth-scroll (- number-lines 1) increment))))

    (global-set-key [(mouse-5)] '(lambda () (interactive) (smooth-scroll 3 1)))
    (global-set-key [(mouse-4)] '(lambda () (interactive) (smooth-scroll 3 -1)))
    )

  ;; Set gui options
  (when window-system

    ;; Default frame size
    (setq default-frame-alist
          '((width . 80)
            (height . 40)))

    ;; Default buffer
    (setq inhibit-startup-screen t)
    (setq initial-buffer-choice nil)

    ;; Set theme -- This looks ugly on terminal, so only use for gui
    (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized/")
    (load-theme 'solarized-dark t)

    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whitespace

(defun set-whitespace-options ()
  ;; Set whitespace settings
  (require 'whitespace)
  (setq whitespace-style
    '(trailing
          empty
          space-after-tab
          space-before-tab
          lines-tail
          indentation face))
  (setq whitespace-action '(auto-cleanup))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode

(defun set-org-mode-options ()
  ;; visual-line and org-indent
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)

  ;(add-to-list 'load-path "~/.emacs.d/elpa/org-20121210/")
  ;(require 'org)
  ;(require 'org-latex)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scala

(defun set-scala-options ()
  (add-to-list 'load-path "~/.emacs.d/elisp/scala-mode")
  (require 'scala-mode-auto)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python

(defun set-python-options ()

  ;; Make running python mode work with packages
  (defun python-reinstate-current-directory ()
    "When running Python, add the current directory ('') to the head
of sys.path.  For reasons unexplained, run-python passes arguments to
the interpreter that explicitly remove '' from sys.path. This means
that, for example, using `python-send-buffer' in a buffer visiting a
module's code will fail to find other modules in the same directory.

Adding this function to `inferior-python-mode-hook' reinstates the
current directory in Python's search path."
    (python-send-string "sys.path[0:0] = ['']"))

  (add-hook 'inferior-python-mode-hook
        'python-reinstate-current-directory)

  (add-hook 'python-mode-hook 'whitespace-mode)
  (add-hook 'python-mode-hook 'flyspell-prog-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell

(defun set-haskell-options ()
  (load "~/.emacs.d/elisp/haskell-mode/haskell-site-file")
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/autocomplete/"))
  (require 'haskell-ac)

  (eval-after-load 'haskell-mode
    '(progn

       (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
       (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
       (add-hook 'haskell-mode-hook 'font-lock-mode)
       (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

       (add-hook 'haskell-mode-hook 'flyspell-prog-mode)
       (add-hook 'haskell-mode-hook 'whitespace-mode)

       ;;(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
       ))

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MATLAB

(defun set-matlab-options ()
  ;; Add matlab support
  (add-to-list 'load-path "~/.emacs.d/elisp/mlab")
  (load-library "matlab-load")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++

(defun set-c++-options ()
  ;; Set C++ style
  (setq c-default-style
    (quote
     ((c-mode . "stroustrup")
      (c++-mode . "stroustrup")
      (java-mode . "java")
      (awk-mode . "awk")
      (other . "gnu"))))

  (add-hook 'c-mode-common-hook 'whitespace-mode)
  (add-hook 'c-mode-common-hook 'flyspell-prog-mode)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX

(defun set-latex-options ()
  ;(add-to-list 'load-path "~/.emacs.d/elpa/auctex-11.86/")
  ;(require 'tex-site)

  (setq LaTeX-command "pdflatex")

  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CS 70

(defun set-cs70-options ()
  ;; Add CS70 Rubric support
  (load "~/.emacs.d/elisp/rubric.el")
  (add-to-list 'auto-mode-alist '("Rubric\.\*\\.txt" . rubric-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Machine

(defun set-machine-options ()

  ;; Detect system
  ; Mac
  (when (eq system-type 'darwin)
    (setq mac-command-modifier (quote super))
    (setq mac-option-modifier (quote meta))
    (setq TeX-view-program-list
          (quote (("Open" "open %s.pdf"))))
    (setq TeX-view-program-selection
          (quote (
                  ((output-dvi style-pstricks) "dvips and gv")
                  (output-dvi "Open")
                  (output-pdf "Open")
                  (output-html "Open"))))
    )

  ;; Check if the extra elisp file exists
  (if (file-exists-p "~/.emacs.d/machine.el")
      (load "~/.emacs.d/machine.el")
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Option groups

(defun kernel-mode ()
  (interactive)

  (setq indent-tabs-mode t)
  (setq tab-width 8)
  (setq c-default-style
        (quote
         ((c-mode . "bsd")
          (c++-mode . "bsd")
          (java-mode . "java")
          (awk-mode . "awk")
          (other . "gnu"))))
  (setq whitespace-action nil)
  (setq whitespace-style
    '(trailing
          empty
          space-after-tab
          space-before-tab
          lines-tail
          indentation face))
  )

(defun grader-mode ()
  (interactive)

  (setq indent-tabs-mode nil)
  (setq tab-width 8)
  (setq whitespace-action nil)
  (setq whitespace-style
    '(space-after-tab
          space-before-tab
          lines-tail
          indentation face))
  )

(defun default-mode ()
  (interactive)

  (set-all-options)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flymake

(defun set-flymake-options ()
  (require 'flymake)
  ;(add-hook 'java-mode-hook 'flymake-mode-on)

  ;(defun my-java-flymake-init ()
  ;  (list "javac" (list (flymake-init-create-temp-buffer-copy
  ;                       'flymake-create-temp-with-folder-structure))))

  ;(add-to-list 'flymake-allowed-file-name-masks
  ;             '("\\.java$" my-java-flymake-init flymake-simple-cleanup))

  (add-to-list 'load-path
               (expand-file-name "~/.emacs.d/elisp/flymake"))
  (require 'flymake-cursor)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autocomplete

(defun set-autocomplete-options ()
  (add-to-list 'load-path "~/.emacs.d/elisp/autocomplete/")
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
  (ac-config-default)

  (add-to-list 'ac-modes 'haskell-mode)
  (ac-flyspell-workaround)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Apply options

(defun set-all-options ()

  (if (< emacs-major-version 24)
      (progn
        (error "This version of emacs is as old as dirt."))
    )

  (set-global-options)

  ;; Mode options

  ; Minor
  (set-flymake-options)
  (set-autocomplete-options)
  (set-whitespace-options)

  ; Major
  (set-org-mode-options)
  (set-cs70-options)
  (set-latex-options)
  (set-python-options)
  (set-haskell-options)
  (set-matlab-options)
  (set-c++-options)
  (set-scala-options)

  ;; Load machine specific options
  (set-machine-options)

  ;; Init package
  (require 'package)
  (package-initialize)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe-mode (quote (0)) nil (fringe))
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-empty ((t (:background "dark slate gray" :foreground "#dc322f"))))
 '(whitespace-trailing ((t (:background "dark slate gray" :foreground "#dc322f" :inverse-video nil :underline nil :slant normal :weight bold)))))
(set-all-options)
