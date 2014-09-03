(defun qiang-comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
If no region is selected and current line is not blank and we are not at the end of the line,
then comment current line.
Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (not (region-active-p))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(defun back-insert-new-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun prev-insert-new-line ()
  (interactive)
  (previous-line)
  (end-of-line)
  (newline-and-indent))

(defun view-in-explorer()
  (interactive)
  (shell-command
   (concat
    "explorer.exe "
    (buffer-file-name))))

(defun run-file ()
  (interactive)
  (load-file (buffer-file-name)))

(defun my-newline-insert-blank-line()
  (newline-and-indent)
  (previous-line)
  (end-of-line)
  (newline-and-indent))

(defun my-newline()
  (interactive)
  (cond ((looking-at ")") (my-newline-insert-blank-line))
        ((looking-at "]") (my-newline-insert-blank-line))
        ((looking-at "}") (my-newline-insert-blank-line))
        (t (newline-and-indent))
        )
  )

(defun duplicate-line ()
  "Duplicate current line."
  (interactive)
  (let ((text (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
        (cur-col (current-column)))
    (end-of-line) (insert "\n" text)
    (beginning-of-line) (right-char cur-col)))

(defun replace-last-sexp ()
    (interactive)
    (let ((value (eval (preceding-sexp))))
      (kill-sexp -1)
      (insert (format "%S" value))))

(provide 'init-utilities)
