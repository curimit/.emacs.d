(require 'helm)

(defun polymer-new ()
  (interactive)
  (let ((path ".."))
    (let ((name (read-input "Component name: ")))
      (let ((dir (concat path "/" name ))
            (jade (concat path "/" name "/" name ".jade"))
            (styl (concat path "/" name "/" name ".styl"))
            (ls (concat path "/" name "/" name ".ls"))
            (bower (concat path "/" name "/" "bower.json"))
            (document (concat path "/" name "/" "document.md"))
            (readme (concat path "/" name "/" "readme.md"))
            (demo (concat path "/" name "/" "demo.jade"))
            (index (concat path "/" name "/" "index.jade"))
            )
        (f-mkdir dir)

        (f-write-text (concat "include ../../global/global.jade

+use('polymer')

include:comment document.md

+polymer-element('" name "')(extends='x-div')
  h1 " name "
") 'utf-8 jade)

        (f-write-text "'use continuation'

Polymer do
  publish: { }
" 'utf-8 ls)

        (f-write-text "@import \"../../global/global.styl\"\n" 'utf-8 styl)

        (f-write-text (concat "# " name "

See the [component page]() for more information.
") 'utf-8 readme)

        (f-write-text (concat "include ../../global/global.jade

doctype html
head
  +polymer-index('" name "')

body(fullbleed)
  " name "(flex)
") 'utf-8 demo)

        (f-write-text (concat "{
  \"name\": \"" name "\",
  \"private\": true,
  \"dependencies\": {
    \"polymer\": \"Polymer/polymer#^0.5\"
  },
  \"version\": \"0.0.0\"
}
") 'utf-8 bower)

        (f-write-text (concat "@element " name "
@extends x-div
@homepage #
@status unstable

This is the document of `" name "`.

### Example:
```
<" name "></" name ">
```
") 'utf-8 document)

        (f-write-text (concat "include ../../global/global.jade

doctype html
head
  +js('webcomponentsjs/webcomponents.js')

  +use('core-component-page')

body(unresolved)
  core-component-page
") 'utf-8 index)

        (find-file jade)
        )
      )
    )
  )

(defun import ()
  (interactive)
  (cond
   ((eq major-mode 'jade-mode)
    (let ((symbol (helm-comp-read "import: " (--map (f-filename it) (f-directories "../../build/website")))))
      (save-excursion
        (beginning-of-buffer)
        (re-search-forward "+use(")
        (backward-char)
        (forward-sexp)
        (backward-char)
        (insert ",\n     '" symbol "'"))))
   )
  )

(defun extends ()
  (interactive)
  (cond
   ((eq major-mode 'jade-mode)
    (let ((component (helm-comp-read "extends-component: " (--filter (f-exists? (concat "../" it "/" it ".jade")) (--map (f-filename it) (f-directories ".."))))))
      (save-excursion
        (beginning-of-buffer)
        (re-search-forward "+polymer-element")
        (end-of-line)
        (insert (concat "(extends=\"" component "\")"))
        )))
   )
  )

(defun switch-component ()
  (interactive)
  (let ((component (helm-comp-read "switch-component: " (--filter (f-exists? (concat "../" it "/" it ".jade")) (--map (f-filename it) (f-directories ".."))))))
    (find-file (concat "../" component "/" component ".jade"))
    (find-file (concat "../" component "/" component ".ls"))
    (find-file (concat "../" component "/" component ".styl"))
    (find-file (concat "../" component "/" component ".jade"))
    )
  )

(provide 'higgs)
