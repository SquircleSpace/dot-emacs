;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global

(defun pull-config ()
  "Fetch configuration from a server"
  (interactive)
  (with-temp-buffer
    (insert-file-contents "/ssh:bradjensen@bradjensen.net:~/.emacs")
    (write-region nil nil user-init-file)))

(defun push-config ()
  "Push configuration to a server"
  (interactive)
  (with-temp-buffer
    (insert-file-contents user-init-file)
    (write-region nil nil "/ssh:bradjensen@bradjensen.net:~/.emacs")))

(defun set-global-options ()

  ;; Set whitespace settings
  (require 'whitespace)
  (setq whitespace-style
	'(trailing empty lines-tail indentation face))
  (setq whitespace-action '(auto-cleanup))

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

  ;; Use aspell for spellchecking
  (setq-default ispell-program-name "aspell"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode

(defun set-org-mode-options ()
  ;; visual-line and org-indent
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)

  ;; Use org-mode 7.7
  (add-to-list 'load-path
	       (expand-file-name "/opt/local/share/emacs/site-lisp"))
  (add-to-list 'load-path
	       (expand-file-name "/opt/local/emacs/site-lisp"))
  (require 'org-install)
  (require 'org-latex)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

  ;; Better subsections for pdf export org-mode
  (require 'org-latex)
  (unless (boundp 'org-export-latex-classes)
    (setq org-export-latex-classes nil))
  (add-to-list 'org-export-latex-classes
	       '("article"
		 "\\documentclass{article}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; Used Minted for better code export
  (setq org-export-latex-listings 'minted)
  ;;(setq org-export-latex-custom-lang-environments
  ;;      '(
  ;;        (emacs-lisp "common-lispcode")
  ;;        ))
  (setq org-export-latex-minted-options
	'(("frame" "lines")
	  ("fontsize" "\\scriptsize")
	  ("linenos" "")))
  (setq org-latex-to-pdf-process
	'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; Org code languages
  ;(org-babel-do-load-languages
  ; 'org-babel-load-languages
  ; '((emacs-lisp . t)
  ;   (R . t)
  ;   (python . t)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python

(defun set-python-options ()

  ;; Make running python mode work with packages
  (defun python-reinstate-current-directory ()
    "When running Python, add the current directory ('') to the head of sys.path.
For reasons unexplained, run-python passes arguments to the
interpreter that explicitly remove '' from sys.path. This means
that, for example, using `python-send-buffer' in a buffer
visiting a module's code will fail to find other modules in the
same directory.

Adding this function to `inferior-python-mode-hook' reinstates
the current directory in Python's search path."
    (python-send-string "sys.path[0:0] = ['']"))
  
  (add-hook 'inferior-python-mode-hook
	    'python-reinstate-current-directory)
  
  (add-hook 'python-mode-hook
	    'whitespace-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

(defun set-other-options ()

  ;; Make shell mode use zsh
  (setq explicit-shell-file-name "/bin/zsh")

  ;; Add matlab support
  (add-to-list 'load-path "~/.emacs.d/mlab")
  (load-library "matlab-load")

  ;; Add CS70 Rubric support
  (add-to-list 'load-path "~/.emacs.d/")
  (load "rubric.el")
  (add-to-list 'auto-mode-alist '("Rubric\.\*\\.txt" . rubric-mode))

  ;; Set C++ style
  (setq c-default-style
	(quote
	 ((c-mode . "stroustrup")
	  (c++-mode . "stroustrup")
	  (java-mode . "java")
	  (awk-mode . "awk")
	  (other . "gnu"))))

  (add-hook 'c-mode-common-hook
	    'whitespace-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Apply options

(defun set-all-options ()
  
  (set-global-options)
  (set-org-mode-options)
  (set-python-options)
  (set-other-options))

(set-all-options)
