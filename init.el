(add-to-list 'load-path "~/.emacs.d/inits")

(require 'init-modern-api)

(require 'init-configs)

(if (file-exists-p "~/.emacs.d/personal.el")
    (load-file "~/.emacs.d/personal.el")
    )
