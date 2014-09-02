;; popup
(add-to-list 'load-path "~/.emacs.d/packages/popup")
(require 'popup)

;; pos-tip
(add-to-list 'load-path "~/.emacs.d/packages/pos-tip")
(require 'pos-tip)

;; popup-pos-tip
(add-to-list 'load-path "~/.emacs.d/packages/popup-pos-tip")
(require 'popup-pos-tip)

;; auto complete mode
(add-to-list 'load-path "~/.emacs.d/packages/auto-complete")
(require 'auto-complete-config)
(ac-config-default)

;; undo tree
(add-to-list 'load-path "~/.emacs.d/packages/undo-tree")
(require 'undo-tree)


(provide 'init-modes)
