;; paredit
;; (require 'paredit)
;; (global-set-key (kbd "M-(") 'paredit-wrap-round)
;; (global-set-key (kbd "M-s") 'paredit-splice-sexp)
;; (global-set-key (kbd "C-<left>") 'paredit-backward-slurp-sexp)
;; (global-set-key (kbd "C-<right>") 'paredit-forward-slurp-sexp)

;; parenface
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

;; litable
(autoload 'litable-mode "litable.el" nil t)

;; popup
(require 'popup)

;; pos-tip
(require 'pos-tip)

;; popup-pos-tip
(require 'popup-pos-tip)
(set-face-attribute 'popup-tip-face    nil   :background "#003A4E" :foreground "light gray")

;; fuzzy
(require 'fuzzy)

;; cedit
(require 'cedet)
(require 'semantic/ia)
(semantic-mode 1)
(global-semanticdb-minor-mode)
(global-semantic-decoration-mode)
(global-semantic-mru-bookmark-mode)
(global-semantic-highlight-func-mode)
(global-semantic-show-unmatched-syntax-mode)
(global-semantic-idle-local-symbol-highlight-mode)
(global-semantic-idle-scheduler-mode)
(global-semantic-idle-summary-mode)
;; (global-semantic-idle-completions-mode)

;; ede
(global-ede-mode)

;; yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/userdata/snippets"
        ))
;; (yas-global-mode 1)

;; auto complete mode
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode)
(setq ac-quick-help-prefer-pos-tip t)
(setq ac-use-quick-help t)
(setq ac-delay 0.5)
(setq ac-candidate-limit 12)
(set-face-attribute 'ac-candidate-face nil   :background "#00222c" :foreground "light gray")
(set-face-attribute 'ac-selection-face nil   :background "SteelBlue4" :foreground "white")
(define-key ac-complete-mode-map (kbd "C-s") 'ac-isearch)

(setq ac-fuzzy-enable t)
(global-set-key (kbd "C-j") 'ac-complete-semantic)

(set-default 'ac-sources
             '(ac-source-semantic
               ac-source-yasnippet
               ac-source-abbrev
               ac-source-words-in-buffer
               ac-source-words-in-all-buffer
               ac-source-imenu
               ac-source-files-in-current-dir
               ac-source-filename))
(add-hook 'c-mode-common-hook '(lambda ()
                                 (add-to-list 'ac-omni-completion-sources
                                              (cons "\\." '(ac-source-semantic)))
                                 (add-to-list 'ac-omni-completion-sources
                                              (cons "->" '(ac-source-semantic)))
                                 (set-default 'ac-sources
                                              '(ac-source-semantic
                                                ac-source-yasnippet
                                                ac-source-abbrev
                                                ac-source-words-in-buffer
                                                ac-source-words-in-all-buffer
                                                ac-source-imenu
                                                ac-source-files-in-current-dir
                                                ac-source-filename))
                                 ))

(defun c++-triple-slash ()
  (interactive)
  (insert "/")
  (let ((doxp nil))
    (save-excursion
      (let ((stop-pos (point)))
        (back-to-indentation)
        (let ((start-pos (point)))
          (let ((str (buffer-substring start-pos stop-pos)))
            (if (and (equal stop-pos (line-end-position))
                     (s-equals-p str "///"))
                (setq doxp 't)
              (setq doxp nil)
              )
            )
          )
        )
      )
    (if doxp
        (let* ((next-func-alist (doxymacs-find-next-func))
               (func-name (cdr (assoc 'func next-func-alist)))
               (params-list (cdr (assoc 'args next-func-alist)))
               (return-name (cdr (assoc 'return next-func-alist)))
               (snippet-text "")
               (idx 1))
          (setq snippet-text (format "// ${1:$(s-repeat (- (-max (--map (length it) (s-split \"\n\" (concat \"// \" yas-text)))) 3) \"-\")}\n // ${1:%s}\n // ${1:$(s-repeat (- (-max (--map (length it) (s-split \"\n\" (concat \"// \" yas-text)))) 3) \"-\")}\n" func-name))
          (setq idx 2)
          (dolist (param params-list)
            (unless (string= param "this")
              (setq snippet-text (concat snippet-text
                                         (format " // @param %s ${%d:}\n" param idx)))
              (setq idx (+ 1 idx))))
          (if (and return-name (not (string= return-name "void")))
              (setq snippet-text (concat snippet-text
                                         (format " // @return ${%d:%s}" idx return-name)))
            (setq snippet-text (substring snippet-text 0 (- (length snippet-text) 1)))
            )
          (backward-delete-char 3)
          (yas/expand-snippet snippet-text))
      )
    )
  )
(add-hook 'c-mode-common-hook (lambda ()
                                (define-key c-mode-base-map (kbd "/") 'c++-triple-slash)
                                ))

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

;; drag stuff
(require 'drag-stuff)
(drag-stuff-global-mode t)

;; sr-speedbar
(require 'sr-speedbar)
(defun sr-speedbar-toggle-and-focus()
  (interactive)
  (sr-speedbar-toggle)
  (if (sr-speedbar-exist-p)
      (sr-speedbar-select-window)))
(global-set-key (kbd "<f8>") 'sr-speedbar-toggle-and-focus)

;; doxymacs
(require 'doxymacs)

;; doxymacs-yard
(require 'doxymacs-yard)

(defun my-doxymacs-font-lock-hook ()
  (interactive)
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (progn (doxymacs-font-lock)
             (doxymacs-yard)
             )))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;; e2wm
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
(require 'window-numbering)
(window-numbering-mode)

;; tabbar
(require 'tabbar)
(tabbar-mode 1)
(global-set-key (kbd "M-h") 'tabbar-backward-tab)
(global-set-key (kbd "M-l") 'tabbar-forward-tab)

(after-load "dired-mode"
  (define-key dired-mode-map (kbd "M-l") 'tabbar-forward-tab)
  )

(after-load "dired+"
  (define-key dired-mode-map (kbd "M-l") 'tabbar-forward-tab)
  )

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
;; (require 'flycheck)
;; (global-flycheck-mode)

;; helm flycheck
;; (require 'helm-flycheck)

;; helm-css-scss
(require 'helm-css-scss)

;; helm-mode-manager
(require 'helm-mode-manager)

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

;; findr
(require 'findr)

;; jump
(require 'jump)

;; rinari
(require 'rinari)

;; coffee-mode
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
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.aspx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))

(after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list "~/.emacs.d/packages//magit"))
(autoload 'magit-status "magit" nil t)

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
         '("if" "else" "for" "in" "each" "case" "when" "default" "block" "extends"
           "name" "value" "mode" "target" "repeat" "selected" "style" "is"
           "one" "two" "three"
           "block append" "block prepend" "append" "prepend"
           "fullbleed" "unresolved" "layout" "reverse"
           "flex" "auto-vertical" "vertical" "auto-horizontal" "horizontal"
           "start-justified" "center-justified" "end-justified" "justified"
           "block" "hidden" "relative" "fit"
           "self-start" "self-center" "self-end" "self-stretch"
           "include" "yield" "mixin") 'words))

  (setq jade-font-lock-keywords
        `((,"!!!\\|doctype\\( ?[A-Za-z0-9\-\_]*\\)?" 0 font-lock-comment-face) ;; doctype
          (,jade-keywords . font-lock-keyword-face) ;; keywords
          (,"\\(on\\(\\-\\w+\\)+\\)" . font-lock-keyword-face) ;; on-*
          (,"\\({{[^{}]+}}\\)" . font-lock-warning-face) ;; data-bindings
          (,"#\\(\\w\\|_\\|-\\)*" . font-lock-variable-name-face) ;; id
          (,"^\\(?:[ {2,}]*\\)\\(\\+[A-Za-z0-9\-\_]*\\)" 1 font-lock-warning-face) ;; mixin
          (,"^\\(?:[ {2,}]*\\(?:[a-z0-9_:\\-]*\\)\\)?\\(#[A-Za-z0-9\-\_]*[^ ]\\)" 1 font-lock-variable-name-face) ;; id
          (,"^\\(?:[ {2,}]*\\(?:[a-z0-9_:\\-]*\\)\\)?\\(\\.[A-Za-z0-9\-\_]*\\)" 1 font-lock-type-face) ;; class name
          (,"^[ {2,}]*[a-z0-9_:\\-]*" 0 font-lock-function-name-face)))
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

;; lua-mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; haskell-mode
(setq haskell-snippets-dir "~/.emacs.d/packages/haskell-mode")
(require 'haskell-mode-autoloads)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(custom-set-variables
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-suggest-remove-import-lines t))

;; ess
(add-to-list 'load-path "~/.emacs.d/packages/ess/lisp")
(require 'ess-site)
(define-key ess-mode-map (kbd "<f10>") 'ess-eval-line-and-step)
(defun r-shell () (interactive) (R))

;; j-mode
(autoload 'j-mode "j-mode.el" "Major mode for editing J files" t)
(add-to-list 'auto-mode-alist '("\\.ij[rstp]$" . j-mode))
(autoload 'j-console "j-mode.el" nil t)
(defun j-shell () (interactive) (j-console))

;; hungry-delete
(require 'hungry-delete)
(global-hungry-delete-mode)
(define-key hungry-delete-mode-map (kbd "C-c C-d") 'hungry-delete-forward)
(define-key hungry-delete-mode-map (kbd "C-c C-<backspace>") 'hungry-delete-backward)

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
;; bookmark+
(require 'bookmark+)

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

;; projectile
(require 'projectile)
(projectile-global-mode t)
(setq projectile-enable-caching t)
(setq projectile-indexing-method 'alien)
(setq projectile-sort-order 'recently-active)
(require 'helm-projectile)
(helm-projectile-on)
(setq projectile-completion-system 'helm)

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
  (if (projectile-project-p)
      (let ((helm-ff-transformer-show-only-basename nil))
        (helm-other-buffer
         '(helm-source-buffers-list
           helm-source-projectile-projects
           helm-source-projectile-files-list
           helm-source-bookmark-files&dirs
           helm-source-recentf
           helm-source-buffer-not-found
           helm-source-bookmark-set
           )
         "*helm search*"))
    (let ((helm-ff-transformer-show-only-basename nil))
      (helm-other-buffer
       '(helm-source-buffers-list
         helm-source-projectile-projects
         helm-source-bookmark-files&dirs
         helm-source-recentf
         helm-source-files-in-current-dir
         helm-source-buffer-not-found
         helm-source-bookmark-set
         )
       "*helm search*"))))

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
         ;; ((projectile-project-p) (projectile-project-p))
         ((buffer-file-name) (f-dirname (buffer-file-name)))
         (t "others"))))

;; git timemachine
(require 'git-timemachine)

;; member-functions
(autoload 'expand-member-functions "member-functions" "Expand C++ member function declarations" t)
(add-hook 'c++-mode-hook (lambda () (local-set-key "\C-cm" #'expand-member-functions)))

;; cpp-buf-utils
(require 'cpp-buf-utils)

;; gist
(require 'gist)

;; helm gist
(require 'helm-gist)

(require 'rainbow-mode)
(setq rainbow-html-colors t)

(after-load "stylus-mode"
  (add-hook 'stylus-mode-hook '(lambda () (rainbow-mode t)))
  )
(after-load "jade-mode"
  (add-hook 'jade-mode-hook '(lambda () (rainbow-mode t)))
  )

;; livescript
(require 'livescript-mode)

(provide 'init-modes)
