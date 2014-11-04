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

(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not
        (with-current-buffer buffer
          (search-forward "warning" nil t))))
      (run-with-timer 1 nil
                      (lambda (buf)
                        (bury-buffer buf)
                        (switch-to-prev-buffer (get-buffer-window buf) 'kill))
                      buffer)))

(defun mirror-view ()
  (interactive)
  (split-window-vertically)
  (other-window 1)
  (mirror-view-mode)
  )

(setq mirror-view-mode-map (make-sparse-keymap))
(define-key mirror-view-mode-map (kbd "C-q") '(lambda ()
                                                (interactive)
                                                (mirror-view-mode 0)
                                                (delete-window)
                                                ))
(define-key mirror-view-mode-map (kbd "C-S-q") '(lambda ()
                                                  (interactive)
                                                  (let ((cursor (point)))
                                                    (mirror-view-mode 0)
                                                    (delete-window)
                                                    (goto-char cursor)
                                                    )
                                                  ))
(define-minor-mode mirror-view-mode
  "Mirror view mode"
  :lighter " mirror"
  :keymap (mirror-view-mode-map))

(defun unicode-symbol (name)
  "Translate a symbolic name for a Unicode character -- e.g., LEFT-ARROW
  or GREATER-THAN into an actual Unicode character code. "
  (decode-char 'ucs (case name
                      ;; arrows
                      ('left-arrow 8592)
                      ('up-arrow 8593)
                      ('right-arrow 8594)
                      ('down-arrow 8595)
                      ;; boxes
                      ('double-vertical-bar #X2551)
                      ;; relational operators
                      ('equal #X003d)
                      ('not-equal #X2260)
                      ('identical #X2261)
                      ('not-identical #X2262)
                      ('less-than #X003c)
                      ('greater-than #X003e)
                      ('less-than-or-equal-to #X2264)
                      ('greater-than-or-equal-to #X2265)
                      ;; logical operators
                      ('logical-and #X2227)
                      ('logical-or #X2228)
                      ('logical-neg #X00AC)
                      ;; misc
                      ('nil #X2205)
                      ('horizontal-ellipsis #X2026)
                      ('double-exclamation #X203C)
                      ('prime #X2032)
                      ('double-prime #X2033)
                      ('for-all #X2200)
                      ('there-exists #X2203)
                      ('element-of #X2208)
                      ;; mathematical operators
                      ('square-root #X221A)
                      ('squared #X00B2)
                      ('cubed #X00B3)
                      ;; letters
                      ('lambda #X03BB)
                      ('alpha #X03B1)
                      ('beta #X03B2)
                      ('gamma #X03B3)
                      ('delta #X03B4))))

(defun substitute-pattern-with-unicode (pattern symbol)
  "Add a font lock hook to replace the matched part of PATTERN with the 
  Unicode symbol SYMBOL looked up with UNICODE-SYMBOL."
  (interactive)
  (font-lock-add-keywords
   nil `((,pattern (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                             ,(unicode-symbol symbol))
                             nil))))))

(defun substitute-patterns-with-unicode (patterns)
  "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
  (mapcar #'(lambda (x)
              (substitute-pattern-with-unicode (car x)
                                               (cdr x)))
          patterns))

(defun clean-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(setq smart-expand-list (make-hash-table :test 'equal))

(defun smart-expand ()
  (interactive)
  (if (not (equal (point) (line-end-position)))
      (error "Cursor should at the end of line.")
    (progn
      (let (line-text space-index cmd rest action)
        (setq line-text (buffer-substring (line-beginning-position) (line-end-position)))
        (setq line-text (s-trim-left line-text))
        (setq space-index (s-index-of " " line-text))

        (if (equal space-index nil)
            (setq cmd line-text
                  args "")
          (setq cmd (substring line-text 0 space-index)
                args (substring line-text (+ 1 space-index))))

        (setq action (gethash cmd smart-expand-list))

        (if (equal action nil)
            (error "Command not found: [%s]" cmd)
          (funcall action args)
          )
        )
      )
    )
  )

(defun smart-expand-define (cmd action)
  (lexical-let ((func action))
    (puthash cmd
             (lambda (raw-args)
               (funcall func raw-args)
               )
             smart-expand-list))
  )

(smart-expand-define "ns"
                     (lambda (raw_args)
                       (let (args)
                         (setq raw_args (s-trim raw_args))
                         (setq args (s-split "\\." raw_args))
                         (back-to-indentation)
                         (kill-line)
                         (--map
                          (progn
                            (insert (concat "namespace "
                                            it
                                            " {"
                                            ))
                            (save-excursion
                              (insert (concat "}  // namespace"
                                              (if (not (equal it "")) " ")
                                              it
                                              ))
                              )
                            (newline-and-indent)
                            (previous-line)
                            (end-of-line)
                            (newline-and-indent)
                            )
                          args)
                         (newline-and-indent)
                         (previous-line)
                         (end-of-line)
                         (newline-and-indent)
                         )
                       ))

(defun helm-do-grep-recursive (&optional non-recursive)
  "Like `helm-do-grep', but greps recursively by default."
  (interactive "P")
  (let* ((current-prefix-arg (not non-recursive))
         (helm-current-prefix-arg non-recursive))
    (call-interactively 'helm-do-grep)))

(provide 'init-utilities)
