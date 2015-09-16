(require 'dash)

(require 's)

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

(defmacro after-load (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

(provide 'init-modern-api)
