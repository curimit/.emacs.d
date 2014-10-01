;; paredit
(add-to-list 'load-path "~/.emacs.d/packages/paredit")
(require 'paredit)
(global-set-key (kbd "M-(") 'paredit-wrap-round)
(global-set-key (kbd "M-s") 'paredit-splice-sexp)
(global-set-key (kbd "C-<left>") 'paredit-backward-slurp-sexp)
(global-set-key (kbd "C-<right>") 'paredit-forward-slurp-sexp)

;; parenface
(add-to-list 'load-path "~/.emacs.d/packages/parenface")
(require 'parenface)
(set-face-foreground 'parenface-paren-face "DarkSlateGray")
(set-face-foreground 'parenface-bracket-face "DarkSlateGray")
(set-face-foreground 'parenface-curly-face "DarkSlateGray")

(defun paren-face-js-add-keyword ()
  "Adds paren-face support to the mode."
  (font-lock-add-keywords nil '(("{\\|}" . parenface-curly-face))))

(defun paren-face-c-add-keyword ()
  "Adds paren-face support to the mode."
  (font-lock-add-keywords nil '(("};\\|{\\|}" . parenface-curly-face))))

(add-hook 'js2-mode-hook 'paren-face-c-add-keyword)
(add-hook 'java-mode-hook 'paren-face-js-add-keyword)
(add-hook 'c-mode-common-hook 'paren-face-c-add-keyword)

;; dash
(add-to-list 'load-path "~/.emacs.d/packages/dash")

;; litable
(add-to-list 'load-path "~/.emacs.d/packages/litable")
(autoload 'litable-mode "litable.el" nil t)

;; popup
(add-to-list 'load-path "~/.emacs.d/packages/popup")
(require 'popup)

;; pos-tip
(add-to-list 'load-path "~/.emacs.d/packages/pos-tip")
(require 'pos-tip)

;; popup-pos-tip
(add-to-list 'load-path "~/.emacs.d/packages/popup-pos-tip")
(require 'popup-pos-tip)
(set-face-attribute 'popup-tip-face    nil   :background "#003A4E" :foreground "light gray")

;; fuzzy
(add-to-list 'load-path "~/.emacs.d/packages/fuzzy")
(require 'fuzzy)

;; cedit
(require 'cedet)
(require 'semantic/ia)
(semantic-mode 1)
(global-semanticdb-minor-mode)
(global-semantic-decoration-mode)
(global-semantic-mru-bookmark-mode)
(global-semantic-idle-local-symbol-highlight-mode)
(global-semantic-idle-scheduler-mode)
(global-semantic-idle-summary-mode)
;; (global-semantic-idle-completions-mode)

;; ede
(global-ede-mode)

;; yasnippet
(add-to-list 'load-path "~/.emacs.d/packages/yasnippet")
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/userdata/snippets"
        ))
(yas-global-mode 1)

;; company mode
(add-to-list 'load-path "~/.emacs.d/packages/company-mode")
(require 'company)
(global-company-mode)
(setq company-idle-delay 0)
(define-key company-active-map (kbd "C-w") 'backward-kill-word)
(set-face-attribute 'company-tooltip nil   :background "#00222c" :foreground "light gray")
(set-face-attribute 'company-tooltip-selection nil   :background "SteelBlue4" :foreground "white")

;; company c headers
(add-to-list 'load-path "~/.emacs.d/packages/company-c-headers")
(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)

;; undo tree
(add-to-list 'load-path "~/.emacs.d/packages/undo-tree")
(require 'undo-tree)
(global-set-key (kbd "C-x C-u") ' undo-tree-visualize)
(global-set-key (kbd "C-/") 'undo-tree-undo)
(global-set-key (kbd "C-?") 'undo-tree-redo)

;; expand region
(add-to-list 'load-path "~/.emacs.d/packages/expand-region")
(require 'expand-region)
(global-set-key (kbd "C-;") 'er/expand-region)

;; multiple cursors
(add-to-list 'load-path "~/.emacs.d/packages/multiple-cursors")
(require 'multiple-cursors)
(global-set-key (kbd "M-<RET>") 'mc/mark-next-like-this)

;; idle-highlight-mode
(add-to-list 'load-path "~/.emacs.d/packages/idle-highlight")
(require 'idle-highlight-mode)
(idle-highlight-mode t)
(set-face-background 'idle-highlight "SlateBlue1")

;; highlight-parentheses
(add-to-list 'load-path "~/.emacs.d/packages/highlight-parentheses")
(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; helm
(add-to-list 'load-path "~/.emacs.d/packages/helm")
(require 'helm-info)
(require 'helm-mode)
(require 'helm-config)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-o") 'helm-bookmarks)
(define-key dired-mode-map (kbd "C-o") 'helm-bookmarks)

(define-key helm-map (kbd "C-w") 'backward-kill-word)

;; helm dash
(add-to-list 'load-path "~/.emacs.d/packages/helm-dash")
(require 'helm-dash)

;; drag stuff
(add-to-list 'load-path "~/.emacs.d/packages/drag-stuff")
(require 'drag-stuff)
(drag-stuff-global-mode t)

;; sr-speedbar
(add-to-list 'load-path "~/.emacs.d/packages/sr-speedbar")
(require 'sr-speedbar)
(defun sr-speedbar-toggle-and-focus()
  (interactive)
  (sr-speedbar-toggle)
  (if (sr-speedbar-exist-p)
      (sr-speedbar-select-window)))
(global-set-key (kbd "<f8>") 'sr-speedbar-toggle-and-focus)

;; doxymacs
(add-to-list 'load-path "~/.emacs.d/packages/doxymacs")
(require 'doxymacs)

;; doxymacs-yard
(add-to-list 'load-path "~/.emacs.d/packages/doxymacs-yard")
(require 'doxymacs-yard)

(defun my-doxymacs-font-lock-hook ()
  (interactive)
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode)) 
      (progn (doxymacs-font-lock)
             (doxymacs-yard)
             )))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;; e2wm
(add-to-list 'load-path "~/.emacs.d/packages/window-layout")
(add-to-list 'load-path "~/.emacs.d/packages/emacs-window-manager")
(require 'e2wm)
(global-set-key (kbd "M-=") 'e2wm:start-management)
(global-set-key (kbd "M--") 'e2wm:stop-management)
(add-hook 'e2wm:def-plugin-imenu-mode-hook (lambda () (linum-mode -1)))
(add-hook 'e2wm:def-plugin-files-mode-hook (lambda () (linum-mode -1)))
(add-hook 'e2wm:def-plugin-history-list-mode-hook (lambda () (linum-mode -1)))
(add-hook 'e2wm:def-plugin-history-list2-mode-hook (lambda () (linum-mode -1)))

(set-face-foreground 'e2wm:face-item "Mediumpurple1")
(set-face-foreground 'e2wm:face-history-list-normal "Mediumpurple1")

;; window-numbering
(add-to-list 'load-path "~/.emacs.d/packages/window-numbering")
(require 'window-numbering)
(window-numbering-mode)

;; tabbar
(add-to-list 'load-path "~/.emacs.d/packages/tabbar")
(require 'tabbar)
(tabbar-mode 1)
(global-set-key (kbd "M-h") 'tabbar-backward)
(global-set-key (kbd "M-l") 'tabbar-forward)

(set-face-attribute 'tabbar-default nil
                    :background "gray80"
                    :foreground "black"
                    :height 1.0
                    )

(set-face-attribute 'tabbar-button nil 
                    :inherit 'tabbar-default
                    )
(set-face-attribute 'tabbar-selected nil
                    :inherit 'tabbar-default
                    :foreground "hotpink"
                    :background "#233b5a"
                    :box '(:line-width 5 :color "#233b5a" :background: "#233b5a")
                    :weight 'bold
                    )
(set-face-attribute 'tabbar-unselected nil
                    :inherit 'tabbar-default
                    :foreground "black"
                    )

;; flycheck
(add-to-list 'load-path "~/.emacs.d/packages/flycheck")
(require 'flycheck)
(global-flycheck-mode)

;; helm flycheck
(add-to-list 'load-path "~/.emacs.d/packages/helm-flycheck")
(require 'helm-flycheck)


;; languages
;; js2-mode
(add-to-list 'load-path "~/.emacs.d/packages/js2-mode")
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; tern
(add-to-list 'load-path "~/.emacs.d/packages/tern/emacs")

(autoload 'tern-mode "tern.el" nil t)

(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

(add-to-list 'load-path "~/.emacs.d/packages/company-tern")
(require 'company-tern)
(add-to-list 'company-backends 'company-tern)

;; enhanced-ruby-mode
(add-to-list 'load-path "~/.emacs.d/packages/Enhanced-Ruby-Mode")
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
;; Manually specify ruby path
;; (setq enh-ruby-program "(path-to-ruby1.9)/bin/ruby")

;; inf-ruby
(add-to-list 'load-path "~/.emacs.d/packages/inf-ruby")
(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)

;; findr
(add-to-list 'load-path "~/.emacs.d/packages/findr")
(require 'findr)

;; jump
(add-to-list 'load-path "~/.emacs.d/packages/jump")
(require 'jump)

;; rinari
(add-to-list 'load-path "~/.emacs.d/packages/rinari")
(require 'rinari)

;; coffee-mode
(add-to-list 'load-path "~/.emacs.d/packages/coffee-mode")
(autoload 'coffee-mode "coffee-mode" nil t)
(add-to-list 'auto-mode-alist
             '("\\.coffee$" . coffee-mode)
             ;; '("\\.coffee$" . rinari-minor-mode)
             )
(defun coffee-custom ()
  "coffee-mode-hook"
  (define-key coffee-mode-map "\C-c\C-c" 'coffee-compile-buffer)
  )
(add-hook 'coffee-mode-hook 'coffee-custom)

;; web mode
(add-to-list 'load-path "~/.emacs.d/packages/web-mode")
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.aspx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))

(add-to-list 'load-path "~/.emacs.d/packages/git-modes")
(add-to-list 'load-path "~/.emacs.d/packages//magit")
(eval-after-load 'info
  '(progn (info-initialize)
          (add-to-list 'Info-directory-list "~/.emacs.d/packages//magit")))
(autoload 'magit-status "magit" nil t)

;; helm ls git
(add-to-list 'load-path "~/.emacs.d/packages/helm-ls-git")
(autoload 'helm-ls-git-ls "helm-ls-git" nil t)

;; diff hl
(add-to-list 'load-path "~/.emacs.d/packages/diff-hl")
(require 'diff-hl)
(set-face-background 'diff-hl-insert "LawnGreen")
(set-face-background 'diff-hl-delete "Red")
(set-face-background 'diff-hl-change "Deepskyblue2")

(provide 'init-modes)
