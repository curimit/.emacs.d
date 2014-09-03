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
             (set-fontset-font (frame-parameter nil 'font) ; 哈哈
                               charset
                               (font-spec :family "Microsoft Yahei" :size 12)))
           )
  (set-face-attribute 'default nil :height 120)
  )

(electric-pair-mode 't)

(require 'tramp)
(setq tramp-default-method "plink")
(setq tramp-default-method "ssh")

;; hippie-expand
(global-set-key [(meta ?/)] 'hippie-expand)

;; no menu
(tool-bar-mode -1)
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

(setq default-major-ode 'text-mode) ;; 缺省mode为text-mode
(setq x-select-enable-clipboard t)  ;; 支持emacs和外部程序之间进行粘贴
(setq make-backup-files nil)        ;; 关闭自动备份功能
(setq backup-inhibited t)           ;; 不产生备份
(setq auto-save-default nil)        ;; 不生成名为#filename#的临时文件
(setq auto-save-mode t)             ;; 自动保存
(global-auto-revert-mode 1)         ;; 自动重载更改的文件
(setq echo-keystrokes 0.1)          ;; 尽快显示按键序列提示
(global-font-lock-mode t)           ;; 语法高亮
(setq auto-image-file-mode t)       ;; 让 Emacs 可以直接打开和显示图片
(setq c-basic-offset 2)             ;; 设置缩进字符数
(put 'erase-buffer 'disabled nil)   ;; 启用清空buffer函数
(auto-image-file-mode)              ;; 支持图片文件显示
(delete-selection-mode t)           ;; 替换选区文字
(fset 'yes-or-no-p 'y-or-n-p)       ;; 以 'y/n'字样代替原默认的'yes/no'字样
(transient-mark-mode t)             ;; 高亮显示要拷贝的区域
(setq inhibit-startup-message t)    ;; 尽快显示按键序列
(setq column-number-mode t)         ;; 显示列号
(setq inhibit-startup-message t)    ;; 不显示GNU emacs启动界面
(which-function-mode t)             ;; 在状态条上显示当前光标在哪个函数体内部

;; Remove this :  Buffer `blah' still has clients; kill it? (yes or no)
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; 使用空格作格式化字符
(setq-default         ;; 使用空格缩进 
 indent-tabs-mode nil ;; t 使用 TAB 作格式化字符  nil 使用空格作格式化字符
 tab-always-indent nil
 tab-width 2)

;; color theme
(add-to-list 'load-path "~/.emacs.d/packages/color-theme")
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

;; replace-region
(global-set-key (kbd "M-%") 'replace-string)

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

(global-set-key (kbd "C--") 'previous-buffer)
(global-set-key (kbd "C-=") 'next-buffer)

(provide 'init-configs)
