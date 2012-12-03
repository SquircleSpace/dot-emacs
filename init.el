;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global

(defun set-global-options ()

  ;; Make shell mode use zsh
  (setq explicit-shell-file-name "/bin/zsh")

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

  ;; Mouse support
  (unless window-system
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

  ;; Use hunspell for spellchecking
  ;(setq-default ispell-program-name "hunspell")
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

  ;; Use private copy of org-mode
  (add-to-list 'load-path
               (expand-file-name "~/.emacs.d/elisp/org-mode/lisp"))
  (require 'org)
  (require 'org-latex)
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

  (add-hook 'python-mode-hook
        'whitespace-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell

(defun set-haskell-options ()
  (load "~/.emacs.d/elisp/haskell-mode/haskell-site-file")
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (add-hook 'haskell-mode-hook 'font-lock-mode)
  (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

  (add-hook 'haskell-mode-hook 'whitespace-mode)

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

  (add-hook 'c-mode-common-hook
        'whitespace-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX

(defun set-latex-options ()
  (add-to-list 'load-path
               (expand-file-name "/opt/local/share/emacs/site-lisp"))
  (require 'tex-site)

  (setq LaTeX-command "pdflatex")
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
    )

  ;; Check if the extra elisp file exists
  (if (file-exists-p "~/.emacs.d/machine.el")
      (load "~/.emacs.d/machine.el")
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GUI

(defun set-gui-options ()

  (when window-system
    ;; Hide tool bar
    (tool-bar-mode -1)

    ;; Hide fringes
    (set-fringe-mode 0)

    ;; Default frame size
    (setq default-frame-alist
          '((width . 80)
            (height . 40)))

    ;; Default buffer
    (setq inhibit-startup-screen t)
    (setq initial-buffer-choice t)
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
;; Apply options

(defun set-all-options ()

  (set-global-options)
  (set-gui-options)

  ;; Mode options
  (set-whitespace-options)
  (set-org-mode-options)
  (set-cs70-options)
  (set-latex-options)

  ;; Programming options
  (set-python-options)
  (set-haskell-options)
  (set-matlab-options)
  (set-c++-options)
  (set-scala-options)

  ;; Load machine specific options
  (set-machine-options)
  )

(set-all-options)
