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

;; auto complete mode
(add-to-list 'load-path "~/.emacs.d/packages/auto-complete")
(require 'auto-complete-config)
(ac-config-default)
(setq ac-quick-help-prefer-pos-tip t)
(setq ac-use-quick-help t)
(setq ac-quick-help-delay 1.0)
(set-face-attribute 'ac-candidate-face nil   :background "#00222c" :foreground "light gray")
(set-face-attribute 'ac-selection-face nil   :background "SteelBlue4" :foreground "white")
(define-key ac-complete-mode-map (kbd "C-s") 'ac-isearch)

(setq ac-fuzzy-enable t)
(global-set-key (kbd "C-<space>") 'ac-fuzzy-complete)

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
(global-semantic-idle-completions-mode)

;; yasnippet
(add-to-list 'load-path "~/.emacs.d/packages/yasnippet")
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/packages/yasnippet/snippets"         ;; the default collection
        ))
(yas-global-mode 1)

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

;; wb-line-number
(add-to-list 'load-path "~/.emacs.d/packages/wb-line-number")
(require 'wb-line-number)
(wb-line-number-toggle)

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

;; drag stuff
(add-to-list 'load-path "~/.emacs.d/packages/drag-stuff")
(require 'drag-stuff)
(drag-stuff-global-mode t)

;; sr-speedbar
(add-to-list 'load-path "~/.emacs.d/packages/sr-speedbar")
(require 'sr-speedbar)
(setq speedbar-use-images nil)
(defun sr-speedbar-toggle-and-focus()
  (interactive)
  (sr-speedbar-toggle)
  (if (sr-speedbar-exist-p)
      (sr-speedbar-select-window)))
(global-set-key (kbd "<f8>") 'sr-speedbar-toggle-and-focus)

;; languages
;; js2-mode
(add-to-list 'load-path "~/.emacs.d/packages/js2-mode")
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

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
             '("\\.coffee$" . rinari-minor-mode)
             '("\\.coffee$" . coffee-mode)
             )
(defun coffee-custom ()
  "coffee-mode-hook"
  (define-key coffee-mode-map "\C-c\C-c" 'coffee-compile-buffer)
  )
(add-hook 'coffee-mode-hook 'coffee-custom)

(provide 'init-modes)
