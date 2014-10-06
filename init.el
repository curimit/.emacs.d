(add-to-list 'load-path "~/.emacs.d/inits")

(require 'init-modern-api)

;; Add all of the directories in packages to load-path
(--map (add-to-list 'load-path it)
       (f-directories "~/.emacs.d/packages"))
      
(require 'init-configs)

(if (file-exists-p "~/.emacs.d/personal.el")
    (load-file "~/.emacs.d/personal.el")
    )
