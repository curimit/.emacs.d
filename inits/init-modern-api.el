(add-to-list 'load-path "~/.emacs.d/packages/dash")
(require 'dash)

(add-to-list 'load-path "~/.emacs.d/packages/s")
(require 's)

(add-to-list 'load-path "~/.emacs.d/packages/f")
(require 'f)

(defmacro linq (result &rest functions)
  "only accept alpha"
  (let ((f nil) (args nil))
    (loop for item in functions do
          ;; check if it is a function
          (if (and (listp item) (equal (car item) 'quote))
              (progn
                (if (not (equal f nil))
                    (progn
                      (if (equal args nil)
                          (setq result (-concat (list f) (list result)))
                        (setq result (-concat (list f) args (list result)))
                        )
                      )
                  )
                (setq f (car (cdr item)))
                (setq args nil)
                )
            (progn
              (setq args (-concat args (list item)))
              )
            )
          )
    (if (not (equal f nil))
        (progn
          (if (equal args nil)
              (setq result (-concat (list f) (list result)))
            (setq result (-concat (list f) args (list result)))
            )
          )
      )
    )
  result
  )

(provide 'init-modern-api)
