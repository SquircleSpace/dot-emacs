; Rubric mode for CS 70
; Last editted on 8/21/12 by Rai Feren <rferen@cs.hmc.edu>
;
; Known Bugs:
;   font-lock does not play nicely with multi-line regexes, so
;   editting a section will break its color.
(defvar check-grades-path "~/bin/check-grades")
(defvar style-check-path "~/bin/style-check")
(defvar comments-path "~/summer12/tools/graderComments.txt")
(defvar given-folder "given/")
(defvar section-re "^\\(\\([A-Za-z0-9 ]*\\)[:.] *Total *= *\\)[^ ]*\\( +/ *[0-9]+\\)")

(defvar positive-comment-font font-lock-doc-face)
(defvar negative-comment-font font-lock-comment-face)
(defvar question-comment-font font-lock-warning-face)

(defvar grader-comment-font font-lock-constant-face)

(defvar section-divider-font font-lock-type-face)
(defvar section-name-font font-lock-type-face)
(defvar subsection-divider-font font-lock-keyword-face)

(defvar rubric-font-lock-defaults
  `((
     ("^\+\+\+>.*$" . positive-comment-font)
     ("^\-\-\->.*$" . negative-comment-font)
     ("^\\?\\?\\?>.*$" . question-comment-font)
     ("^=+\n\\([^\n]*\n\\)*=+$" . section-divider-font)
     ("^\\(\\([A-Za-z0-9 ]*\\)[:.] *Total *= *\\)[^ ]*\\( +/ *[0-9]+\\)" 
      . section-name-font)
     ("^=+\n$" . section-divider-font)
     ("^-+\n\\([A-Za-z][^\n]*\n\\)*-+$" . subsection-divider-font)
     ("^|.*$" . grader-comment-font)
     )))

; For manual score setting, in case check-grades is broken O_o
; Probably could go, as is probably more likely to cause confusion, but eh.
(defun rubric-man-set-section-score ()
  (interactive)
  (save-excursion
  (re-search-backward section-re)
  (let* ((section-name (match-string 2))
         (new-score (read-from-minibuffer 
                     (format "Enter score for section %s: " section-name))))
    (rubric-prog-set-section-score section-name new-score))))

; Calls two helpers to get the score and then set it.
; WILL NOT WORK IF YOU HAVEN'T SAVED.
(defun rubric-auto-set-section-score ()
  (interactive)
  (save-excursion
  (re-search-backward section-re)
  (let* (
        (section-name (match-string 2))
        (score (rubric-check-grades-section section-name)))
    (message (format "Found score of %s for %s" score section-name))
    (rubric-prog-set-section-score section-name score))))

; prog refers to how it should be only called by programs.
; Sets the following types of lines:
;
; ================================
; SECTION. Total <value> = 42
; ================================
;
; and
;
; Your Scores:
;    SECTION    =   <value> / 42
(defun rubric-prog-set-section-score (section score)
  (beginning-of-buffer)
  (re-search-forward (concat "^\\(.*" section ".*Total *= *\\)"
                              "[^ ]*\\( +/ *[0-9]+\\)"))
  (let ((first-part (match-string 1)) 
        (last-part (match-string 2)))
    (beginning-of-line)
    (kill-line)
    (insert (format "%s%s%s" first-part score last-part)))
  (re-search-forward (concat "^\\( *" section-name " * =\\)[^/$]*"
                             "\\(/ *[0-9]+\\).*$"))
  (let ((start-your-scores (match-string 1))
        (end-your-scores   (match-string 2)))
    (beginning-of-line)
    (kill-line)
    (insert (format "%s   %s %s" 
                    start-your-scores score end-your-scores)))
  (message (format "Changing %s to %s" section score)))

; Call check-grades to get score for a section.
; YOU NEED TO SAVE BEFORE THIS WORKS.
(defun rubric-check-grades-section (section)
  (shell-command-to-string (concat check-grades-path " -f=Rubric.txt -g " section)))

; Dumps results of check-grades into a buffer.
; Again, NEED TO SAVE BEFORE USING THIS
(defun rubric-check-grades ()
  (interactive)
  (call-process check-grades-path nil "check-grades.out" t "-f=./Rubric.txt")
  (display-buffer "check-grades.out" t)
)

; Gets the combined score via check-grades
; AS USUAL, SAVE BEFORE THIS
(defun rubric-check-grades-calc ()
  (shell-command-to-string (concat check-grades-path " -c -f=Rubric.txt"))
)

; NEED TO SAVE BEFORE USING THIS.
; Sets the following line:
;
; =======================
; COMBINED SCORE: <value>
; =======================
(defun rubric-combined-score ()
  (interactive)
  (save-excursion
  (end-of-buffer)
  (re-search-backward "^\\(COMBINED SCORE:\\)")
  (let ((first-part (match-string 1)))
        (beginning-of-line)
        (let ((new-score (rubric-check-grades-calc)))
          (kill-line)
          (message (format "%s %s" first-part new-score))
          (insert (format "%s %s" first-part new-score))))))

; Dumps the comment archive into another window.
; Its assumed graders can use C-s and the like to get which one they
; want, as its pretty unambiguous.
(defun rubric-grader-comment ()
  (interactive)
  (find-file-other-window comments-path))

; Dumps style-check output into another window.
; WARNING: I don't think this will work for HW1 because it looks at
; only the current folder by default.
(defun rubric-style-check ()
  (interactive)
  (call-process style-check-path nil "style-check.out" t)
  (display-buffer "style-check.out" t)
)

; Opens the original rubric in a new window.
; Unlike rubric-grader-comment, this WILL move you to a relevant section.
; This is done 'cause unlike rubric-grader-comment, there isn't much
; reason to want non-relevant areas (you might want the
; assignment-agnostic comments, for instance in the previous)
(defun rubric-get-intervals ()
  (interactive)
  (let ((filepath (buffer-file-name)))
    (message filepath)
    ; Get the subsection, or just make something up
    (if (re-search-backward "^\\(-+\n[A-z  -\*]+\n-+\\)$" nil t 1)
        (setq subsection (match-string 1))
      (setq subsection ""))
    ; Get where the original rubric is
    (string-match "^\\(.*/\\)\\(?:grading\\|test\\)/\\(hw\[0-9\]+\\)/.*$" 
                  filepath)
    (let* ((path (match-string 1 filepath))
           (assignment (match-string 2 filepath))
           (orig-rubric (concat path given-folder assignment 
                                "/Rubric.txt")))
      (message (concat "Opening original rubric at: " orig-rubric))
      (find-file-other-window orig-rubric)

      (message (concat "Looking for subsection:\n" subsection))
      (goto-char (point-min))
      (if (search-forward subsection nil t 1)
          nil
        (error "Search failed.")))
    )
)

(define-derived-mode rubric-mode
  text-mode "Rubric.txt"
  "Major mode for CS 70 Rubric.txt
\\{rubric-mode-map}"
  (setq mode-name "CS70-Rubric")
  (setq font-lock-multiline t)
  (setq font-lock-defaults rubric-font-lock-defaults)
  (auto-fill-mode)
  (set-fill-column 79)
;  (set-fill-prefix "\|") ; RAI: This just seems to throw errors... O_o
  )

(define-key rubric-mode-map (kbd "C-c q") 'rubric-man-set-section-score)
(define-key rubric-mode-map (kbd "C-c s") 'rubric-auto-set-section-score)
(define-key rubric-mode-map (kbd "C-c t") 'rubric-style-check)
(define-key rubric-mode-map (kbd "C-c c") 'rubric-check-grades)
(define-key rubric-mode-map (kbd "C-c f") 'rubric-combined-score)
(define-key rubric-mode-map (kbd "C-c g") 'rubric-grader-comment)
(define-key rubric-mode-map (kbd "C-c v") 'rubric-get-intervals)