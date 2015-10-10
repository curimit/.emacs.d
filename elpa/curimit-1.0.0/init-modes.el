
;; rainbow-mode
(add-to-list 'auto-mode-alist '("\\.css$" . rainbow-mode))

(require 'pcache)

;; parenface
(after-load "parenface"
  (defun paren-face-js-add-keyword ()
    "Adds paren-face support to the mode."
    (set-face-foreground 'paren-face "DarkSlateGray")
    (font-lock-add-keywords nil '(("{\\|}" . paren-face))))

  (defun paren-face-c-add-keyword ()
    "Adds paren-face support to the mode."
    (set-face-foreground 'paren-face "DarkSlateGray")
    (font-lock-add-keywords nil '(("};\\|{\\|}" . paren-face))))

  (add-hook 'js2-mode-hook 'paren-face-c-add-keyword)
  (add-hook 'java-mode-hook 'paren-face-js-add-keyword)
  (add-hook 'c-mode-common-hook 'paren-face-c-add-keyword)
  )
(require 'parenface)

;; popup-pos-tip
(after-load "popup-pos-tip"
  (set-face-attribute 'popup-tip-face nil
                      :background "#003A4E" :foreground "light gray"))
(require 'popup-pos-tip)



(require 'company)
(global-company-mode)
(setq company-idle-delay 0.2)
(define-key company-active-map (kbd "C-w") 'backward-kill-word)

(require 'flycheck)
(global-flycheck-mode)

(if (boundp 'ycmd-path)
    (progn
      (require 'ycmd)
      (setq ycmd-idle-change-delay 0.5)
      (global-ycmd-mode)

      (set-variable 'ycmd-server-command (list "python" ycmd-path))

      (require 'company-ycmd)
      (company-ycmd-setup)

      (require 'flycheck-ycmd)
      (flycheck-ycmd-setup)))

;; undo tree
(require 'undo-tree)
(global-set-key (kbd "C-x C-u") ' undo-tree-visualize)
(global-set-key (kbd "C-/") 'undo-tree-undo)
(global-set-key (kbd "C-?") 'undo-tree-redo)

;; expand region
(require 'expand-region)
(global-set-key (kbd "C-;") 'er/expand-region)

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "M-<RET>") 'mc/mark-next-like-this)

;; idle-highlight-mode
(require 'idle-highlight-mode)
(idle-highlight-mode t)
(set-face-background 'idle-highlight "SlateBlue1")

;; highlight-parentheses
(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; helm
(require 'helm-info)
(require 'helm-mode)
(require 'helm-config)
(require 'helm-misc)
(require 'helm-command)
(require 'helm-ring)
(require 'helm-color)
(require 'helm-imenu)

(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(define-key helm-map (kbd "C-w") 'backward-kill-word)
(define-key helm-map (kbd "C-k") 'kill-line)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-M-p") 'helm-previous-source)
(define-key helm-map (kbd "C-M-n") 'helm-next-source)

;; use helm-find-files as default
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key helm-find-files-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "<right>") 'helm-select-action)
(define-key helm-find-files-map (kbd "C-k") 'kill-line)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

;; helm dash
(require 'helm-dash)
(defun ensure-dash-docset (list)
  (setq-local helm-dash-docsets list)
  )

(defun c++-doc ()
  (interactive)
  (ensure-dash-docset '("C++" "C"))
  )
(add-hook 'c-mode-common-hook 'c++-doc)

(defun emacs-lisp-doc ()
  (interactive)
  (ensure-dash-docset '("Emacs Lisp"))
  )
(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-doc)

(setq helm-dash-min-length 0)

(helm-dash-buffer-local-docsets)


;; helm flycheck
;; (require 'helm-flycheck)

;; helm-css-scss
(require 'helm-css-scss)

;; helm-mode-manager
;; (require 'helm-mode-manager)

;; helm gist
(require 'helm-gist)


;; drag stuff
(after-load "drag-stuff"
  (drag-stuff-global-mode t))
(require 'drag-stuff)

;; sr-speedbar
(after-load "sr-speedbar"
  (defun sr-speedbar-toggle-and-focus()
    (interactive)
    (sr-speedbar-toggle)
    (if (sr-speedbar-exist-p)
        (sr-speedbar-select-window)))
  (global-set-key (kbd "<f8>") 'sr-speedbar-toggle-and-focus)
  )
(require 'sr-speedbar)

;; e2wm
;; (global-set-key (kbd "M-=") 'e2wm:start-management)
;; (global-set-key (kbd "M--") 'e2wm:stop-management)
;; (add-hook 'e2wm:def-plugin-imenu-mode-hook (lambda () (linum-mode -1)))
;; (add-hook 'e2wm:def-plugin-files-mode-hook (lambda () (linum-mode -1)))
;; (add-hook 'e2wm:def-plugin-history-list-mode-hook (lambda () (linum-mode -1)))
;; (add-hook 'e2wm:def-plugin-history-list2-mode-hook (lambda () (linum-mode -1)))

;; (set-face-foreground 'e2wm:face-item "Mediumpurple1")
;; (set-face-foreground 'e2wm:face-history-list-normal "Mediumpurple1")

;; window-numbering
(require 'window-numbering)
(window-numbering-mode)

;; tabbar
(after-load "tabbar"
  (tabbar-mode 1)
  (global-set-key (kbd "M-h") 'tabbar-backward-tab)
  (global-set-key (kbd "M-l") 'tabbar-forward-tab)

  (set-face-attribute 'tabbar-default nil
                      :background "gray80"
                      :foreground "black"
                      :height 1.0)

  (after-load "dired-mode"
    (define-key dired-mode-map (kbd "M-l") 'tabbar-forward-tab))

  (after-load "dired+"
    (define-key dired-mode-map (kbd "M-l") 'tabbar-forward-tab))

  (set-face-attribute 'tabbar-button nil
                      :inherit 'tabbar-default)

  (set-face-attribute 'tabbar-selected nil
                      :inherit 'tabbar-default
                      :foreground "hotpink"
                      :background "#233b5a"
                      :box '(:line-width 5 :color "#233b5a" :background: "#233b5a")
                      :weight 'bold)

  (set-face-attribute 'tabbar-unselected nil
                      :inherit 'tabbar-default
                      :foreground "black")
  )
(require 'tabbar)


;; flycheck
(require 'flycheck)
(global-flycheck-mode)

;; languages
;; js2-mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; tern
(add-to-list 'load-path "~/.emacs.d/packages/tern/emacs")
(autoload 'tern-mode "tern.el" nil t)

(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

;; enhanced-ruby-mode
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
;; Manually specify ruby path
;; (setq enh-ruby-program "(path-to-ruby1.9)/bin/ruby")

;; inf-ruby
(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)

;; ;; coffee-mode
;; (autoload 'coffee-mode "coffee-mode" nil t)
;; (add-to-list 'auto-mode-alist
;;              '("\\.coffee$" . coffee-mode)
;;              ;; '("\\.coffee$" . rinari-minor-mode)
;;              )
;; (defun coffee-custom ()
;;   "coffee-mode-hook"
;;   (define-key coffee-mode-map "\C-c\C-c" 'coffee-compile-buffer)
;;   )
;; (add-hook 'coffee-mode-hook 'coffee-custom)

;; web mode
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.aspx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))

;; helm ls git
(autoload 'helm-ls-git-ls "helm-ls-git" nil t)

;; diff hl
(require 'diff-hl)
(set-face-background 'diff-hl-insert "LawnGreen")
(set-face-background 'diff-hl-delete "Red")
(set-face-background 'diff-hl-change "Deepskyblue2")

;; git gutter fringe
(require 'git-gutter-fringe)
(global-git-gutter-mode 1)

;; back button
(require 'back-button)
(back-button-mode 1)
(global-set-key (kbd "C--") 'back-button-local-backward)
(global-set-key (kbd "C-=") 'back-button-local-forward)
(global-set-key (kbd "C-\\") 'back-button-push-mark-local-and-global)

;; anzu
(require 'anzu)
(global-anzu-mode t)

(custom-set-variables
 '(anzu-deactivate-region t)
 '(anzu-search-threshold 1000)
 '(anzu-replace-to-string-separator " => "))

(set-face-attribute 'anzu-mode-line nil
                    :foreground "red" :weight 'bold)

(global-set-key (kbd "M-%") 'anzu-query-replace)

;; auto pair
;; (require 'autopair)
;; (autopair-global-mode nil)
;; (setq autopair-blink nil)

(electric-pair-mode t)

;; feature-mode
(autoload 'feature-mode "feature-mode" nil t)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
(setq feature-default-i18n-file "~/.emacs.d/packages/feature-mode/i18n.yml")

;; nummm-mode
(require 'nummm-mode)

;; simple-httpd
(require 'simple-httpd)

;; skewer
(require 'skewer-mode)

(autoload 'skewer-css-mode "skewer-css" nil t)
(autoload 'skewer-html-mode "skewer-html" nil t)
(autoload 'skewer-repl "skewer-repl" nil t)

(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'web-mode-hook 'skewer-html-mode)

(require 'htmlize)

(autoload 'stylus-mode "stylus-mode" nil t)
(autoload 'jade-mode "jade-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.styl$" . stylus-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

(after-load "jade-mode"
  (setq jade-keywords
        (regexp-opt
         '(
           "around-justified" "block" "center" "center-center" "center-justified" "end" "end-justified" "fit"
           "fixed-bottom" "fixed-left" "fixed-right" "fixed-top" "flex" "flex-1" "flex-10" "flex-11" "flex-12"
           "flex-2" "flex-3" "flex-4" "flex-5" "flex-6" "flex-7" "flex-8" "flex-9" "flex-auto" "flex-none"
           "fullbleed" "horizontal" "horizontal-reverse" "inline" "invisible" "justified" "layout" "relative"
           "scroll" "self-center" "self-end" "self-start" "self-stretch" "start" "start-justified" "vertical"
           "vertical-reverse" "wrap" "wrap-reverse"

           "if" "else" "for" "in" "each" "case" "when" "default" "block" "extends"
           "block append" "block prepend" "append" "prepend"
           "include" "yield" "mixin") 'words))

  (setq jade-font-lock-keywords
        `((,"!!!\\|doctype\\( ?[A-Za-z0-9\-\_]*\\)?" 0 font-lock-comment-face) ;; doctype
          (,"\\bon\\-\\w+='\\w+'" . font-lock-warning-face) ;; data-bindings on-*='XXX'
          (,"\\({{[^{}]+}}\\)" . font-lock-warning-face) ;; data-bindings {{ }}
          (,"\\(\\[\\[[^][]+\\]\\]\\)" . font-lock-warning-face) ;; data-bindings [[ ]]
          (,jade-keywords . font-lock-keyword-face) ;; keywords
          (,"\"\\([^\"]+\\)\"" . font-lock-string-face)
          (,"'\\([^']+\\)'" . font-lock-string-face)
          ;; \\(=?\\bon[-a-z]+='\\)\\w+
          (,"#\\(\\w\\|_\\|-\\)*" . font-lock-variable-name-face) ;; id
          (,"\"\\([^\"]+\\)\"" . font-lock-constant-face)
          (,"^\\(?:[ {2,}]*\\)\\(\\+[A-Za-z0-9\-\_]*\\)" 1 font-lock-warning-face) ;; mixin
          (,"^\\(?:[ {2,}]*\\(?:[a-zA-Z0-9_:\\-]*\\)\\)?\\(#[A-Za-z0-9\-\_]*[^ ]\\)" 1 font-lock-variable-name-face) ;; id
          (,"^\\(?:[ {2,}]*\\(?:[.a-zA-Z0-9_:\\-]*\\)\\)?\\(\\.[A-Za-z0-9\-\_]*\\)" 1 font-lock-type-face) ;; class name
          (,"^\\(?:[ {2,}]*\\(?:[a-zA-Z0-9_:\\-]*\\)\\)?\\(\\.[A-Za-z0-9\-\_]*\\)" 1 font-lock-type-face) ;; class name
          (,"\\([A-Za-z0-9$-]+\\)=" 1 font-lock-type-face) ;; attribute
          (,"^[ {2,}]*[a-zA-Z0-9_:\\-]*" 0 font-lock-function-name-face)))
  )

(global-set-key (kbd "M-g") 'helm-imenu)

(define-key helm-map (kbd "C-j") (lambda ()
                                   (interactive)
                                   (helm-next-line)
                                   (helm-execute-persistent-action)
                                   ))
(define-key helm-map (kbd "C-k") (lambda ()
                                   (interactive)
                                   (helm-previous-line)
                                   (helm-execute-persistent-action)
                                   ))

;; hungry-delete
(require 'hungry-delete)
(global-set-key (kbd "C-c C-d") 'hungry-delete-forward)
(global-set-key (kbd "C-c C-<backspace>") 'hungry-delete-backward)

;; dired+
(require 'dired+)

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
                                  (interactive) (dired-sort-other (concat dired-listing-switches ""))))

                             (define-key dired-mode-map (kbd "C-o") 'wg-switch-to-workgroup)
                             (define-key dired-mode-map (kbd "C-s") '(lambda ()
                                                                       (interactive)
                                                                       (helm-swoop :$query "")))
                             ))
;; super-smart-ops
;; (require 'super-smart-ops)
;; (super-smart-ops-configure-for-mode 'c++-mode :rem '(":"))
;; (super-smart-ops-configure-for-mode 'js2-mode)

;; helm swoop
(require 'helm-swoop)
(define-key helm-swoop-map (kbd "C-k") 'kill-line)

(defun my-helm-swoop ()
  (interactive)
  (if (use-region-p)
      (helm-swoop)
    (helm-swoop :$query "")))

(global-set-key (kbd "M-i") 'my-helm-swoop)
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

;; hideshowvis
(require 'hideshowvis)
(dolist (hook (list 'emacs-lisp-mode-hook
                    'js2-mode-hook
                    'c-mode-common-hook))
  (add-hook hook 'hideshowvis-enable))
(hideshowvis-symbols)
(global-set-key (kbd "C-c C-f") 'hs-toggle-hiding)
(set-face-background hs-face "DeepPink2")

;; aggressive-indent-mode
;; (require 'aggressive-indent)
;; (global-aggressive-indent-mode 1)
;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode)

;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; sunrise commander
(autoload 'sunrise "sunrise-commander" nil t)

;; workgroups2
(require 'workgroups2)
(setq wg-prefix-key (kbd "C-z"))
(setq wg-session-file "~/.emacs.d/.workgroups")
(setq wg-emacs-exit-save-behavior 'save)
(setq wg-workgroups-mode-exit-save-behavior 'save)
(workgroups-mode 1)

;; Use helm as default engine for wg-completing-read
(defun wg-completing-read (prompt choices &optional pred require-match initial-input history default)
  (helm--completing-read-default prompt choices pred require-match
                                 initial-input history default))
(global-set-key (kbd "C-o") 'wg-switch-to-workgroup)


(defun helm-dwim ()
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm-other-buffer
     '(helm-source-buffers-list
       helm-source-bookmark-files&dirs
       helm-source-recentf
       helm-source-files-in-current-dir
       helm-source-buffer-not-found
       helm-source-bookmark-set
       )
     "*helm search*")))

(global-set-key (kbd "C-x C-b") 'helm-dwim)

;; customize tabbar group function
(defun tabbar-buffer-groups ()
  (list (cond
         ((s-starts-with-p "*helm" (buffer-name)) "helm buffer")
         ((s-starts-with-p "*scratch" (buffer-name)) "scratch buffer")
         ((or (equal major-mode 'dired-mode)
              (equal major-mode 'wdired-mode)) "dired buffer")
         (buffer-read-only "readonly buffer")
         ((s-starts-with-p "*" (buffer-name)) "emacs buffer")
         ((equalp (buffer-file-name) nil) "noname buffer")
         ((buffer-file-name) (f-dirname (buffer-file-name)))
         (t "others"))))

;; git timemachine
(require 'git-timemachine)

;; member-functions
(autoload 'expand-member-functions "member-functions" "Expand C++ member function declarations" t)
(add-hook 'c++-mode-hook (lambda () (local-set-key "\C-cm" #'expand-member-functions)))

(require 'rainbow-mode)
(setq rainbow-html-colors t)

(after-load "jade-mode"
  (add-hook 'jade-mode-hook '(lambda () (rainbow-mode t)))
  )

;; livescript
(require 'livescript-mode)

;; AUCTEX
(after-load "tex-mik"
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-PDF-mode t)

  ;; Use Skim as viewer, enable source <-> PDF sync
  ;; make latexmk available via C-c C-c
  ;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (push
                                '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
                                  :help "Run latexmk on file")
                                TeX-command-list)))
  (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "pdflatex")))
  )

(require 'one-ui)

(require 'indent-guide)
(indent-guide-global-mode)

(provide 'init-modes)
