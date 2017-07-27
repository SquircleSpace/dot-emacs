;; Bar cursor
(set-default 'cursor-type 'bar)

;; Show Column numbers
(setf column-number-mode t)

;; Show file size
(size-indication-mode t)

;; No menu bar
(menu-bar-mode -1)

;; Set gui options
(when window-system

  ;; Default frame size
  (setf default-frame-alist
        '((width . 80)
          (height . 40)))

  ;; Default buffer
  (setf inhibit-startup-screen t)
  (setf initial-buffer-choice nil)

  ;; No fringe... unless they wanted one.
  (unless (featurep 'fringe-config)
    (set-fringe-mode 0))

  (set-scroll-bar-mode nil)

  (tool-bar-mode 0))

;; No bell
;; Do nothing
(defun silent-bell ())
(setf ring-bell-function 'silent-bell)
;; Flash
;;(setf visible-bell t)

;; Don't jump when cursor goes out of screen
(setf scroll-conservatively 10000)

;; Delete selected text
(pending-delete-mode 1)

;; Overwrite selected text
(delete-selection-mode 1)

;; Highlight when jumping the cursor
(use-package volatile-highlights
  :demand t
  :config
  (progn
    (defadvice pop-tag-mark (after highlight-line)
      (save-excursion
	(let ((line-start (progn (move-beginning-of-line 1) (point)))
	      (line-end (progn (move-end-of-line 1) (1+ (point)))))
	  (vhl/add-range line-start line-end))))
    (ad-activate 'pop-tag-mark)

    (defadvice recenter-top-bottom (after highlight-line)
      (save-excursion
	(let ((line-start (progn (move-beginning-of-line 1) (point)))
	      (line-end (progn (move-end-of-line 1) (1+ (point)))))
	  (vhl/add-range line-start line-end))))
    (ad-activate 'recenter-top-bottom)))

(provide 'ui-config)
