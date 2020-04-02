(package-initialize)

;; init packages
(setq package-required-packages
      '(helm
        helm-swoop
        multiple-cursors
        expand-region
        color-theme-modern
        dash
        f
        nlinum
        drag-stuff
        treemacs
        sublime-themes
        smart-hungry-delete
        company
        company-quickhelp
        magit
        restclient
        back-button
        csharp-mode
        omnisharp))

(if (eval (cons 'and (mapcar (lambda(package) (package-installed-p package)) package-required-packages)))
    (require 'package)
  (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                           ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

  (package-refresh-contents)

  (mapcar (lambda(package) (package-install package)) package-required-packages)
  )

;; add load-path for ~/.emacs.d/packages
(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))
(add-subdirs-to-load-path "~/.emacs.d/packages")

;; color theme
(if (eq window-system nil)
    nil
  (progn
    (require 'color-theme-modern)
    (load-theme 'spolsky t t)
    (enable-theme 'spolsky)
    )
  )

(put 'set-goal-column 'disabled nil)

;; default encoding
(setq default-buffer-file-coding-system 'utf-8)

;; hippie-expand
(global-set-key [(meta ?/)] 'hippie-expand)

;; no menu
(if (not (eq system-type 'darwin))
    (tool-bar-mode -1))
(scroll-bar-mode -1)

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

(setq tab-width 4)

(defun prev-insert-new-line ()
  (interactive)
  (previous-line)
  (end-of-line)
  (newline-and-indent))

(defun back-insert-new-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent)
  )

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

(global-set-key "\M-;" 'qiang-comment-dwim-line)

(defun duplicate-line ()
  "Duplicate current line."
  (interactive)
  (let ((text (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
        (cur-col (current-column)))
    (end-of-line) (insert "\n" text)
    (beginning-of-line) (right-char cur-col)))

(global-set-key (kbd "C-M-d") 'duplicate-line)

(global-set-key (kbd "C-x C-m") 'execute-extended-command)

(global-set-key (kbd "C-;") 'er/expand-region)
(global-set-key (kbd "M-<RET>") 'mc/mark-next-like-this)

(require 'gcl-mode)

;; aweshell
(require 'aweshell)
(global-set-key (kbd "C-'") 'aweshell-next)
(global-set-key (kbd "C-\"") 'aweshell-prev)
(global-set-key (kbd "C-M-'") 'aweshell-new)

;; csharp-mode
(require 'csharp-mode)

;; nox
(require 'nox)
(add-to-list 'nox-server-programs '(csharp-mode . ("OmniSharp")))

;; awesome-tab
(require 'awesome-tab)
(awesome-tab-mode t)
(global-set-key (kbd "M-h") 'awesome-tab-backward-tab)
(global-set-key (kbd "M-l") 'awesome-tab-forward-tab)
(setq awesome-tab-dark-unselected-blend 0.2)
(setq awesome-tab-dark-selected-blend 0.2)
(setq awesome-tab-active-bar-height 36)

(require 'f)

(defun awesome-tab-buffer-groups ()
  (list
   (cond
    ((or (string-equal "*" (substring (buffer-name) 0 1))
         (memq major-mode '(magit-process-mode
                            magit-status-mode
                            magit-diff-mode
                            magit-log-mode
                            magit-file-mode
                            magit-blob-mode
                            magit-blame-mode
                            )))
     "Emacs")
    ((derived-mode-p 'eshell-mode)
     "EShell")
    ((derived-mode-p 'emacs-lisp-mode)
     "Elisp")
    ((derived-mode-p 'dired-mode)
     "Dired")
    ((memq major-mode '(org-mode org-agenda-mode diary-mode))
     "OrgMode")
    ((buffer-file-name) (f-dirname (buffer-file-name)))
    (t
     (awesome-tab-get-group-name (current-buffer))))))

(setq awesome-tab-height 120)
(setq awesome-tab-display-icon nil)

;; snails
(require 'snails)
(global-set-key (kbd "C-c C-p") 'snails)

;; nlinum
(global-nlinum-mode)
(defun my-nlinum-mode-hook ()
  (when nlinum-mode
    (setq-local nlinum-format " %d ")))
(add-hook 'nlinum-mode-hook #'my-nlinum-mode-hook)

;; drag stuff
(require 'drag-stuff)
(global-set-key (kbd "M-<up>")   #'drag-stuff-up)
(global-set-key (kbd "M-<down>") #'drag-stuff-down)

;; helm
(require 'helm)
(require 'helm-files)
(require 'helm-command)
(define-key helm-map (kbd "C-w") 'backward-kill-word)
(define-key helm-map (kbd "C-k") 'kill-line)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(define-key helm-map "\t" 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-M-p") 'helm-previous-source)
(setq helm-ff-kill-or-find-buffer-fname-fn 'ignore)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

(require 'helm-swoop)
(global-set-key (kbd "M-i") 'helm-swoop)
(define-key helm-swoop-map (kbd "C-w") 'backward-kill-word)
(define-key helm-swoop-map (kbd "C-k") 'kill-line)

;; treemacs
(require 'treemacs)
(global-set-key [f8] 'treemacs)
(defun my-treemacs-mode-hook ()
  (nlinum-mode -1))
(add-hook 'treemacs-mode-hook #'my-treemacs-mode-hook)

(require 'smart-hungry-delete)
(global-set-key (kbd "C-c C-w") 'smart-hungry-delete-backward-char)
(global-set-key (kbd "C-c C-d") 'smart-hungry-delete-forward-char)

;; tramp
(setq tramp-default-method "ssh")

;; back-button
;; (require 'back-button)
(back-button-mode 1)
(global-set-key (kbd "C--") 'back-button-local-backward)
(global-set-key (kbd "C-=") 'back-button-local-forward)

;; company
(require 'company)
(global-company-mode)
(global-set-key (kbd "C-SPC") 'company-complete)
(define-key company-active-map (kbd "C-w") 'backward-kill-word)
(company-quickhelp-mode)

;; ominisharp
(add-hook 'csharp-mode-hook 'omnisharp-mode)
(eval-after-load
 'company
 '(add-to-list 'company-backends 'company-omnisharp))

(add-hook 'csharp-mode-hook #'company-mode)

;; local config
(if (f-exists-p "~/.emacs.d/local.el")
    (load-file "~/.emacs.d/local.el"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (omnisharp company-quickhelp restclient back-button magit company-1 csharp-mode company smart-hungry-delete sublime-themes treemacs drag-stuff nlinum f dash color-theme-modern expand-region multiple-cursors helm-swoop helm zenburn-theme yasnippet-snippets yaml-mode which-key undo-tree rust-mode puppet-mode lv lsp-ui ido-completing-read+ graphviz-dot-mode goto-chg gitignore-mode gitconfig-mode gitattributes-mode git-modes folding ess diminish csv-mode company-lsp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
