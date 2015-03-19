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
;; (menu-bar-mode -1)

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

;; color theme
(require 'color-theme)
(after-load "color-theme"
  (color-theme-initialize)
  (color-theme-dark-blue2))

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

(setq gud-key-prefix "\C-x\C-g")

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

(defvar my-mode-line-buffer-line-count nil)
(make-variable-buffer-local 'my-mode-line-buffer-line-count)

(defun my-mode-line-count-lines ()
  (setq my-mode-line-buffer-line-count (int-to-string (count-lines (point-min) (point-max)))))

(add-hook 'find-file-hook 'my-mode-line-count-lines)
(add-hook 'after-save-hook 'my-mode-line-count-lines)
(add-hook 'after-revert-hook 'my-mode-line-count-lines)
(add-hook 'dired-after-readin-hook 'my-mode-line-count-lines)

(setq frame-title-format
      (list "[" '(:eval (projectile-project-name)) "]"
            " "
            (user-full-name)
            " @ "
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))


(setq show-buffer-file-name nil)
(defun toggle-show-buffer-file-name ()
  "toggle show or hide buffer full file name in mode line"
  (interactive)
  (setq show-buffer-file-name
        (if show-buffer-file-name nil t)))
(global-set-key (kbd "M-<f11>") 'toggle-show-buffer-file-name)

;;====================== time setting =====================
(display-time-mode 1)

(setq display-time-24hr-format t)
(setq display-time-format "%02H:%02M:%02S %Y-%02m-%02d %3a")

(setq display-time-day-and-date t)

(setq display-time-interval 1)

(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
;;----------------------    END    time setting    ---------------------

(defun my-mode-line ()
  (setq-default
   mode-line-format
   (list

    "["
    '(:eval (window-numbering-get-number-string))
    "] "

    '(:eval (propertize "%b " 'face 'font-lock-keyword-face
                        'help-echo (format "%s" (buffer-file-name))
                        ))

    "[" ;; insert vs overwrite mode, input-method in a tooltip
    '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                        'face 'font-lock-preprocessor-face
                        'help-echo (concat "Buffer is in "
                                           (if overwrite-mode "overwrite" "insert") " mode")))

    ;; was this buffer modified since the last save?
    '(:eval (when (buffer-modified-p)
              (concat ","  (propertize "Mod"
                                       'face 'font-lock-warning-face
                                       'help-echo "Buffer has been modified"))))

    ;; is this buffer read-only?
    '(:eval (when buffer-read-only
              (concat ","  (propertize "RO"
                                       'face 'font-lock-type-face
                                       'help-echo "Buffer is read-only"))))
    "] "

    ;; the current major mode for the buffer.
    "["

    '(:eval (propertize "%m" 'face 'font-lock-string-face
                        'help-echo buffer-file-coding-system))

    "] "

    ;; project name
    '(:eval (format "[%s]" (projectile-project-name)))

    '(:eval (when vc-mode
              (concat "["
                      (propertize (string-strip (format "%s" vc-mode)) 'face 'font-lock-variable-name-face)
                      "] "
                      )))

    ;; workspace name
    '(:eval (propertize (wg-mode-line-string) 'face 'font-lock-type-face))
    " "

    "["
    (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
    "] "

    ;; relative position, size of file
    "["
    (propertize "%I" 'face 'font-lock-constant-face) ;; size
    '(:eval (when (and (not (buffer-modified-p)) my-mode-line-buffer-line-count)
              (propertize (concat " / " my-mode-line-buffer-line-count "L")
                          'face 'font-lock-type-face
                          )))
    "] "

    ;; add the time, with the date and the emacs uptime in the tooltip
    '(:eval (propertize (format-time-string "[%H:%M:%S]")
                        'face 'font-lock-type-face
                        'help-echo
                        (concat (format-time-string "%Y-%02m-%02d %02H:%02M:%02S %Y-%02m-%02d %3a; ")
                                (emacs-uptime "Uptime:%hh"))))

    ;; show buffer file name
    '(:eval (when show-buffer-file-name
              (format " [%s]" (buffer-file-name))))

    " "

    ;; date
    '(:eval (propertize (format-time-string "[%Y-%02m-%02d %3a]")
                        'face 'font-lock-comment-face))

    ;; ;; line and column
    " (" ;; '%02' to set to 2 chars at least; prevents flickering
    (propertize "%01l" 'face 'font-lock-type-face) ","
    (propertize "%02c" 'face 'font-lock-type-face)
    ") "
    ))
  )
(my-mode-line)

(set-face-foreground 'mode-line "white")
(set-face-background 'mode-line "gray15")
(set-face-foreground 'mode-line-inactive "white")
(set-face-background 'mode-line-inactive "gray45")
(set-face-foreground 'window-numbering-face "OrangeRed")
(set-face-bold-p 'window-numbering-face 't)
(set-face-foreground 'wg-mode-line-face "OrangeRed")
(set-face-bold-p 'wg-mode-line-face 't)

;; org-mode
(setq org-hide-leading-stars t)

;; c++ header use c++-mode
(add-to-list 'auto-mode-alist '("\\.h$'" . c++-mode))

;; clean-buffer
(global-set-key (kbd "C-S-l") 'clean-buffer)

;; goto-line
(global-set-key (kbd "s-g") 'goto-line)

(require 'recentf)

(defun reopen-last-killed-file ()
  (interactive)
  (let ((active-files (loop for buf in (buffer-list)
                            when (buffer-file-name buf) collect it)))
    (loop for file in recentf-list
          unless (member file active-files) return (find-file file))))

(define-key global-map (kbd "C-S-t") 'reopen-last-killed-file)

;; import
(define-key global-map (kbd "C-c C-i") 'import)
(define-key global-map (kbd "C-c C-o") 'import-component)

;; switch
(define-key global-map (kbd "C-S-O") 'switch-component)

(provide 'init-configs)
