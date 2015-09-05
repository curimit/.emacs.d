(put 'set-goal-column 'disabled nil)

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

;; hippie-expand
(global-set-key [(meta ?/)] 'hippie-expand)

;; no menu
(menu-bar-mode -1)

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
(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq auto-save-mode nil)
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
(setq-default word-wrap t)

;; Remove this :  Buffer `blah' still has clients; kill it? (yes or no)
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

(setq-default
 indent-tabs-mode nil
 tab-always-indent nil
 tab-width 2)

(require 'init-utilities)
(require 'init-modes-lite)

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

(setq gud-key-prefix "\C-x\C-g")

(global-set-key (kbd "RET") 'my-newline)

(global-set-key "\M-;" 'qiang-comment-dwim-line)

(global-set-key (kbd "C-M-0") 'forward-sexp)
(global-set-key (kbd "C-M-9") 'backward-sexp)

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
   (list (cons "\\(>=\\)" 'greater-than-or-equal-to)
         (cons "\\(<=\\)" 'less-than-or-equal-to))))

(add-hook 'c-mode-common-hook 'c++-unicode)

(defun js2-unicode ()
  (interactive)
  (substitute-patterns-with-unicode
   (list (cons "\\(>=\\)" 'greater-than-or-equal-to)
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

;; smart-expand
(add-hook 'c-mode-common-hook (lambda ()
                                (define-key c-mode-base-map (kbd "C-c C-j") 'smart-expand)
                                ))

;; coding style: 80 chars limit
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
(add-hook 'c++-mode-hook 'whitespace-mode)

;; delete trailing-whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'c-mode-common-hook (lambda ()
                                (define-key c-mode-base-map (kbd "<f12>") 'semantic-ia-fast-jump)
                                (define-key c-mode-base-map (kbd "<M-f12>") 'semantic-analyze-proto-impl-toggle)
                                (define-key c-mode-base-map (kbd "C-<f12>") 'ff-find-other-file)
                                (define-key c-mode-base-map (kbd "<f7>") 'compile)
                                ))

;; cursors following mouse
(setq mouse-autoselect-window t)

;; import
(define-key global-map (kbd "C-c C-i") 'import)

;; switch
(define-key global-map (kbd "C-S-O") 'switch-component)

(provide 'init-configs-lite)