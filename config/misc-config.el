(use-package diminish)

;; Inhibit the startup screen
(setq inhibit-startup-screen t)

;; Don't polute everything with backups
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups/"))))

;; Don't polute home directory with extra files, either
(setq save-place-file (concat user-emacs-directory "saved-places"))

;; Always add new line
(setq require-final-newline t)

;; No tab characters
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

;; Fix shift up
(defadvice terminal-init-xterm (after select-shift-up activate)
  (define-key input-decode-map "\e[1;2A" [S-up]))

;; Use SSH when tramping.
(setq tramp-default-method "ssh")

;; Make scrolling in docview mode continuous
(setq doc-view-continuous t)

;; Support unfill
(use-package unfill)

;; Undo tree
(use-package undo-tree
  :demand t
  :config
  (progn
    (global-undo-tree-mode 1)
    (diminish 'undo-tree-mode)))

;; Smartparens
(use-package smartparens
  :demand t
  :config
  (progn
    (setf sp-autoskip-closing-pair t)
    (sp-use-paredit-bindings)
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1)
    (setf sp-highlight-pair-overlay nil)
    (diminish 'smartparens-mode)))

;; Easier window motion
(use-package windmove
  :ensure nil
  :demand t
  :config
  (windmove-default-keybindings (if (eq system-type 'darwin) 'super 'meta)))
(use-package framemove
  :if window-system
  :demand t
  :config
  (setf framemove-hook-into-windmove t))

;; Highlight recent changes
(use-package volatile-highlights
  :demand t
  :config
  (volatile-highlights-mode t)
  :diminish volatile-highlights-mode)

;; Harrumph.  Stop dragging in files!
(setf dired-dnd-protocol-alist nil)

;; Enable commands that aren't confusing
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Begin code from Emacs Prelude
;; Copy line
(defadvice kill-ring-save (before smart-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-end-position)))))

;; Cut line
(defadvice kill-region (before smart-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap beginning-of-visual-line]
                'prelude-move-beginning-of-line)
(global-set-key [remap move-beginning-of-line]
                'prelude-move-beginning-of-line)

(defun prelude-sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
;; End Emacs prelude code

(use-package define-word
  :bind (("M-#" . define-word-at-point)))

(provide 'misc-config)
