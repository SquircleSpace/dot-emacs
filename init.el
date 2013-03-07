;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global

(defun set-global-options ()

  ;; Make shell mode use zsh
  (setq explicit-shell-file-name "/bin/zsh")

  ;; Hide menu bar
  (menu-bar-mode -1)

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


  ;; Set gui options
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

  (add-hook 'python-mode-hook 'whitespace-mode)
  (add-hook 'python-mode-hook 'flyspell-prog-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell

(defun set-haskell-options ()
  (load "~/.emacs.d/elisp/haskell-mode/haskell-site-file")
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (add-hook 'haskell-mode-hook 'font-lock-mode)
  (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

  (add-hook 'haskell-mode-hook 'flyspell-prog-mode)
  (add-hook 'haskell-mode-hook 'whitespace-mode)

  (add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/autocomplete/"))
  (require 'haskell-ac)

  (eval-after-load 'haskell-mode
    '(progn
       (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
       ))

  (defun my-haskell-mode-hook ()
    (setq ac-sources
          (append '(ac-source-yasnippet
                    ac-source-abbrev
                    ac-source-words-in-buffer
                    my/ac-source-haskell)
                  ac-sources))

    (dolist (x '(haskell literate-haskell))
      (add-hook
       (intern (concat (symbol-name x)
                       "-mode-hook"))
       'turn-on-paredit))
    )

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
  (add-to-list 'load-path
               (expand-file-name "/opt/local/share/emacs/site-lisp"))
  (require 'tex-site)

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
;; Minimap

(defun set-minimap-options ()
  (add-to-list 'load-path
               (expand-file-name "~/.emacs.d/elisp/minimap"))
  (require 'minimap)
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

  (set-global-options)

  ;; Mode options

  ; Minor
  (set-minimap-options)
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
  )

(set-all-options)
