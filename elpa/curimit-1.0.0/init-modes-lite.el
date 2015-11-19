;; parenface
(require 'parenface)
(set-face-foreground 'paren-face "DarkSlateGray")

(defun paren-face-js-add-keyword ()
  "Adds paren-face support to the mode."
  (font-lock-add-keywords nil '(("{\\|}" . paren-face))))

(defun paren-face-c-add-keyword ()
  "Adds paren-face support to the mode."
  (font-lock-add-keywords nil '(("};\\|{\\|}" . paren-face))))

(add-hook 'js2-mode-hook 'paren-face-c-add-keyword)
(add-hook 'java-mode-hook 'paren-face-js-add-keyword)
(add-hook 'c-mode-common-hook 'paren-face-c-add-keyword)

;; undo tree
(autoload 'undo-tree-visualize "undo-tree.el" nil t)
(global-set-key (kbd "C-x C-u") ' undo-tree-visualize)

(autoload 'undo-tree-undo "undo-tree.el" nil t)
(global-set-key (kbd "C-/") 'undo-tree-undo)

(autoload 'undo-tree-redo "undo-tree.el" nil t)
(global-set-key (kbd "C-?") 'undo-tree-redo)

;; expand region
(autoload 'er/expand-region "expand-region.el" nil t)
(global-set-key (kbd "C-;") 'er/expand-region)

;; multiple cursors
(global-set-key (kbd "M-<RET>") 'mc/mark-next-like-this)
(autoload 'mc/mark-next-like-this "multiple-cursors.el" nil t)

;; drag stuff
(require 'drag-stuff)
(drag-stuff-global-mode t)

;; tabbar
(require 'tabbar)
(tabbar-mode 1)
(global-set-key (kbd "M-h") 'tabbar-backward-tab)
(global-set-key (kbd "M-l") 'tabbar-forward-tab)

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

(after-load "dired-mode"
  (define-key dired-mode-map (kbd "M-l") 'tabbar-forward-tab))

(after-load "dired+"
  (define-key dired-mode-map (kbd "M-l") 'tabbar-forward-tab))

;; web mode
(autoload 'web-mode "web-mode" "" t)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.aspx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))

(autoload 'magit-status "magit" nil t)

;; anzu
(autoload 'anzu-query-replace "anzu" nil t)
(after-load 'anzu
  (custom-set-variables
   '(anzu-deactivate-region t)
   '(anzu-search-threshold 1000)
   '(anzu-replace-to-string-separator " => "))

  (set-face-attribute 'anzu-mode-line nil
                      :foreground "red" :weight 'bold)
  )


(global-set-key (kbd "M-%") 'anzu-query-replace)

(electric-pair-mode t)

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
                             ))

;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; smex
(autoload 'smex "smex"
  "Smex is a M-x enhancement for Emacs, it provides a convenient interface to
your recently and most frequently used commands.")
(global-set-key (kbd "C-x C-m") 'smex)

;; ido
(autoload 'ibuffer "ibuffer" nil t)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; bash completion
(bash-completion-setup)

(add-hook 'shell-dynamic-complete-functions
          'bash-completion-dynamic-complete)

;; eshell-z
(require 'eshell-z)


(provide 'init-modes-lite)
