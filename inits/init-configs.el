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
             (set-fontset-font (frame-parameter nil 'font) ; ����
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

(setq default-major-ode 'text-mode) ;; ȱʡmodeΪtext-mode
(setq x-select-enable-clipboard t)  ;; ֧��emacs���ⲿ����֮�����ճ��
(setq make-backup-files nil)        ;; �ر��Զ����ݹ���
(setq backup-inhibited t)           ;; ����������
(setq auto-save-default nil)        ;; ��������Ϊ#filename#����ʱ�ļ�
(setq auto-save-mode t)             ;; �Զ�����
(global-auto-revert-mode 1)         ;; �Զ����ظ��ĵ��ļ�
(setq echo-keystrokes 0.1)          ;; ������ʾ����������ʾ
(global-font-lock-mode t)           ;; �﷨����
(setq auto-image-file-mode t)       ;; �� Emacs ����ֱ�Ӵ򿪺���ʾͼƬ
(setq c-basic-offset 2)             ;; ���������ַ���
(put 'erase-buffer 'disabled nil)   ;; �������buffer����
(auto-image-file-mode)              ;; ֧��ͼƬ�ļ���ʾ
(delete-selection-mode t)           ;; �滻ѡ������
(fset 'yes-or-no-p 'y-or-n-p)       ;; �� 'y/n'��������ԭĬ�ϵ�'yes/no'����
(transient-mark-mode t)             ;; ������ʾҪ����������
(setq inhibit-startup-message t)    ;; ������ʾ��������
(setq column-number-mode t)         ;; ��ʾ�к�
(setq inhibit-startup-message t)    ;; ����ʾGNU emacs��������
(which-function-mode t)             ;; ��״̬������ʾ��ǰ������ĸ��������ڲ�

;; Remove this :  Buffer `blah' still has clients; kill it? (yes or no)
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; ʹ�ÿո�����ʽ���ַ�
(setq-default         ;; ʹ�ÿո����� 
 indent-tabs-mode nil ;; t ʹ�� TAB ����ʽ���ַ�  nil ʹ�ÿո�����ʽ���ַ�
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
