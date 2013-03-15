;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global

(defun set-global-options ()

  ;; iswitch-buffer is the best
  (iswitchb-mode)

  ;; Bar cursor
  (set-default 'cursor-type 'bar)

  ;; Make shell mode use zsh
  (setq explicit-shell-file-name "/bin/zsh")

  ;; Use word-count mode
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/"))
  (autoload 'wc-mode "wc-mode" nil t)

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

    ;; No menu bar
    (menu-bar-mode -1)

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

  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
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
  (eval-after-load 'auto-complete
    '(progn
       (add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/"))
       (require 'haskell-ac)
       (require 'auto-complete-config)
       (add-to-list 'ac-modes 'haskell-mode)
       )
    )
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'font-lock-mode)
  (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
  
  (add-hook 'haskell-mode-hook 'flyspell-prog-mode)
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

  (add-hook 'c-mode-common-hook 'whitespace-mode)
  (add-hook 'c-mode-common-hook 'flyspell-prog-mode)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX

(defun set-latex-options ()
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
    ;; keyboard stuff
    (setq mac-command-modifier (quote super))
    (setq mac-option-modifier (quote meta))
    (global-set-key [(super a)] 'mark-whole-buffer)
    (global-set-key [(super v)] 'yank)
    (global-set-key [(super c)] 'kill-ring-save)
    (global-set-key [(super x)] 'kill-region)
    (global-set-key [(super s)] 'save-buffer)
    (global-set-key [(super l)] 'goto-line)
    (global-set-key [(super w)]
                    (lambda () (interactive) (delete-window)))
    (global-set-key [(super z)] 'undo)
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
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autocomplete

(defun set-autocomplete-options ()
  (require 'auto-complete-config)
  (ac-config-default)
  (ac-flyspell-workaround)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management

(defvar needed-packages
  '(auctex auto-complete flymake flymake-cursor
           haskell-mode org rainbow-mode undo-tree magit
           color-theme-solarized))

(defun install-packages ()
  (message "%s" "Packages missing. Refreshing...")
  
  (package-refresh-contents)

  (message "%s" "done. Installing...")

  (dolist (p needed-packages)
    (when (not (package-installed-p p))
      (package-install p)))
  )

(defun all-packages-installed ()
  (require 'cl)
  (loop for p in needed-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

(defun set-package-options ()
  ;; Init package
  (require 'package)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)

  (when (not (all-packages-installed))
    (install-packages))

  ;; What does this do?
  ;(package-menu--find-upgrades)
  
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Apply options

(defun set-all-options ()

  (if (< emacs-major-version 24)
      (progn
        (error "This version of emacs is as old as dirt."))
    )

  (set-package-options)

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

  ;; Load machine specific options
  (set-machine-options)

  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" default)))
 '(fringe-mode (quote (0)) nil (fringe))
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
