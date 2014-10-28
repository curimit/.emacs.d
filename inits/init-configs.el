(put 'set-goal-column 'disabled nil)

;; Title
;; (setq frame-title-format "Emacs@%b")

(setq default-frame-alist
      '((top . 0) (left . 0)
        (width . 120) (height . 45)))

(setq initial-frame-alist '((top . 10) (left . 30)))

;; default encoding
(setq default-buffer-file-coding-system 'utf-8)

;; Font
(if (eq system-type 'windows-nt)
    (progn (set-face-attribute 'default nil :font "Consolas 11")
           (dolist (charset '(kana han symbol cjk-misc bopomofo))
             (set-fontset-font (frame-parameter nil 'font)
                               charset
                               (font-spec :family "Microsoft Yahei" :size 12)))
           )
  (set-face-attribute 'default nil :height 120)
  )

(require 'tramp)
(setq tramp-default-method "plink")
(setq tramp-default-method "ssh")

;; hippie-expand
(global-set-key [(meta ?/)] 'hippie-expand)

;; no menu
(if (not (eq system-type 'darwin))
    (tool-bar-mode -1))
(scroll-bar-mode -1)
;(menu-bar-mode -1)

;; hight paren
(show-paren-mode nil)
(set-face-foreground 'show-paren-match "red")
(set-face-bold-p 'show-paren-match t)
(set-face-background 'show-paren-match "green")
(setq show-paren-style 'parentheses)

;; enable recursive in minibuffer
(setq enable-recursive-minibuffers t)

;; scroll-margin 3
(setq scroll-margin 3 scroll-conservatively 10000)

(setq default-major-ode 'text-mode)
(setq x-select-enable-clipboard t)
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq auto-save-mode t)
(global-auto-revert-mode 1)
(setq echo-keystrokes 0.1)
(global-font-lock-mode t)
(setq auto-image-file-mode t)
(setq c-basic-offset 2)
(put 'erase-buffer 'disabled nil)
(auto-image-file-mode)
(delete-selection-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(transient-mark-mode t)
(setq inhibit-startup-message t)
(setq column-number-mode t)
(setq inhibit-startup-message t)
(which-function-mode t)

;; Remove this :  Buffer `blah' still has clients; kill it? (yes or no)
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

(setq-default
 indent-tabs-mode nil
 tab-always-indent nil
 tab-width 2)

;; color theme
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-dark-blue2)))

(require 'init-utilities)
(require 'init-modes)

;; keyboard bindings
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-x C-n") 'set-mark-command)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key [C-S-return] 'prev-insert-new-line)
(global-set-key [C-return] 'back-insert-new-line)
(global-set-key (kbd "C-x C-v") 'view-in-explorer)
(global-set-key (kbd "<C-S-tab>") 'insert-one-tab)
(global-set-key (kbd "C-x C-a") 'align-regexp)
(global-set-key (kbd "<C-backspace>") 'replace-last-sexp)
(global-set-key (kbd "<f9>") 'run-file)

(global-set-key (kbd "RET") 'my-newline)

(global-set-key "\M-;" 'qiang-comment-dwim-line)

(global-set-key (kbd "C-M-0") 'forward-sexp)
(global-set-key (kbd "C-M-9") 'backward-sexp)

;; Eval-expression
;; (global-set-key (kbd "M-!") 'eval-expression)

(defun up-slightly () (interactive) (scroll-up 3))
(defun down-slightly () (interactive) (scroll-down 3))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)

(defun scroll-up-1 () (interactive) (scroll-up 1))
(defun scroll-down-1 () (interactive) (scroll-down 1))
(global-set-key (kbd "C-<up>") 'scroll-down-1)
(global-set-key (kbd "C-<down>") 'scroll-up-1)

(global-set-key (kbd "C-M-d") 'duplicate-line)

;; compile
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)
(add-hook 'c-mode-common-hook #'(lambda ()
                                (define-key c-mode-base-map [(f7)] 'compile)
                                ))
(setq compilation-scroll-output t)
(setq compilation-auto-jump-to-first-error t)

(global-linum-mode)
(set-face-foreground 'linum "orange")
(setq linum-format "%5d ")

;; mirror view
(global-set-key (kbd "M-M") 'mirror-view)
(add-hook 'js2-mode-hook #'(lambda ()
                             (interactive)
                             (local-set-key (kbd "M-S-m") 'mirror-view)
                             ))

;; unicode display
(defun c++-unicode ()
    (interactive)
    (substitute-patterns-with-unicode
     (list (cons "\\(==\\)" 'identical)
           (cons "\\(!=\\)" 'not-identical)
           (cons "\\(&&\\)" 'logical-and)
           (cons "\\(||\\)" 'logical-or)
           (cons "\\(>=\\)" 'greater-than-or-equal-to)
           (cons "\\(<=\\)" 'less-than-or-equal-to))))
  
(add-hook 'c-mode-common-hook 'c++-unicode)

(defun js2-unicode ()
    (interactive)
    (substitute-patterns-with-unicode
     (list (cons "\\(===\\)" 'identical)
           (cons "\\(!==\\)" 'not-identical)
           (cons "\\(function\\)" 'lambda)
           (cons "\\(&&\\)" 'logical-and)
           (cons "\\(||\\)" 'logical-or)
           (cons "\\(>=\\)" 'greater-than-or-equal-to)
           (cons "\\(<=\\)" 'less-than-or-equal-to))))
  
(add-hook 'js2-mode-hook 'js2-unicode)

(defun lisp-unicode ()
    (interactive)
    (substitute-patterns-with-unicode
     (list (cons "\\(lambda\\)" 'lambda))))

(add-hook 'emacs-lisp-mode-hook 'lisp-unicode)

;; compilation window settings
(setq compilation-scroll-output t)
(setq compilation-auto-jump-to-first-error t)
(setq compilation-auto-jump-to-next t)
(setq compilation-always-kill t)

;; sort dired
(add-hook 'dired-mode-hook (lambda ()
                             (interactive)
                             (make-local-variable  'dired-sort-map)
                             (setq dired-sort-map (make-sparse-keymap))
                             (define-key dired-mode-map "s" dired-sort-map)
                             (define-key dired-sort-map "s"
                               '(lambda () "sort by Size"
                                  (interactive) (dired-sort-other (concat dired-listing-switches "S"))))
                             (define-key dired-sort-map "x"
                               '(lambda () "sort by eXtension"
                                  (interactive) (dired-sort-other (concat dired-listing-switches "X"))))
                             (define-key dired-sort-map "t"
                               '(lambda () "sort by Time"
                                  (interactive) (dired-sort-other (concat dired-listing-switches "t"))))
                             (define-key dired-sort-map "n"
                               '(lambda () "sort by Name"
                                  (interactive) (dired-sort-other (concat dired-listing-switches ""))))))


(provide 'init-configs)
