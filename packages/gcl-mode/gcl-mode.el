(defvar gcl-indent-offset 4
  "*Indentation offset for `gcl-mode'.")

(defun gcl-indent-line ()
  "Indent current line for `gcl-mode'."
  (interactive)
  (let ((indent-col 0))
    (save-excursion
      (beginning-of-line)
      (condition-case nil
          (while t
            (backward-up-list 1)
            (when (looking-at "[[{(=]")
              (setq indent-col (+ indent-col gcl-indent-offset))))
        (error nil)))
    (save-excursion
      (back-to-indentation)
      (when (and (looking-at "[]})]") (>= indent-col gcl-indent-offset))
        (setq indent-col (- indent-col gcl-indent-offset))))
    (indent-line-to indent-col)))

(setq gcl-highlights
      '(("\\(#.*\\)" (1 'font-lock-comment-face))
        ("\\b\\(import\\|def\\|assert\\|as\\|local\\|optimize\\|with_weight\\)\\b" . font-lock-constant-face)
        ("\\b\\(true\\|false\\|null\\)\\b" . font-lock-constant-face)
        ("\\b\\(Universe\\|ConsoleReport\\|CsvReport\\|TradeReport\\|SimpleCombine\\)\\b" . font-lock-function-name-face)
        ("\\b\\(Matrix\\|Vector\\|TimeIndexVector\\|Dates\\|Instruments\\|Path\\)\\b" . font-lock-function-name-face)
        ("\\b\\(AlphaOp[a-zA-Z]+\\)\\((\\)" . (1 font-lock-function-name-face))
        ("\\b\\(int\\|double\\|bool\\|string\\)\\b" . font-lock-builtin-face)
        ("\\b\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\([\\t ]*=\\)" (1 font-lock-variable-name-face))
        ("\\(\\b\\|-\\)[0-9]+\\b" . font-lock-constant-face)
        ("\"\\(\\(?:.\\|\n\\)*?[^\\]\\)\"" . font-lock-string-face)
        ))

(define-derived-mode gcl-mode text-mode "gcl-mode"
  "Mode for editing some kind of config files."
  (setq font-lock-defaults '(gcl-highlights))
  (setq comment-start "#")
  ;; (setq comment-end "")
  ;; (setq comment-start-skip "[ \t]*")
  ;; (setq comment-end-skip "")
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  ;; (make-local-variable 'gcl-indent-offset)
  ;; (set (make-local-variable 'indent-line-function) 'insert-tab)
  )

(autoload 'gcl-mode "gcl" "Gcl mode." t)
(add-to-list 'auto-mode-alist '("\\.gcl\\'" . gcl-mode))

(provide 'gcl-mode)
