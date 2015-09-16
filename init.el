(add-to-list 'load-path "~/.emacs.d/inits")

(require 'package)

(global-set-key (kbd "C-x C-\\") 'package-install)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(require 'curimit)
