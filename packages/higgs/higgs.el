(require 'helm)

(defun f-write-file (name text)
  (f-write-text text 'utf-8 name))

(defun polymer-new ()
  (interactive)
  (let ((path ".."))
    (let ((name (read-input "Component name: ")))
      (let ((dir (concat path "/" name))
            (demo-dir (concat path "/" name "/demo"))
            (test-dir (concat path "/" name "/test"))
            (index (concat path "/" name "/" "index.jade"))
            (jade (concat path "/" name "/" name ".jade"))
            (styl (concat path "/" name "/" name ".styl"))
            (ls (concat path "/" name "/" name ".ls"))
            (bower (concat path "/" name "/" "bower.json"))
            (readme (concat path "/" name "/" "readme.md"))
            (demo (concat path "/" name "/" "demo/index.jade"))
            (test-index (concat path "/" name "/" "test/index.jade"))
            (test-ls (concat path "/" name "/" "test/test.ls"))
            )
        (f-mkdir dir)
        (f-mkdir demo-dir)
        (f-mkdir test-dir)

        ;; index
        (f-write-file index (concat "include ../../global/global.jade

html
  head
    meta(charset='utf-8')
    meta(name='viewport' content='width=device-width, initial-scale=1.0')

    +js('../webcomponentsjs/webcomponents-lite.js')
    +import('../iron-component-page/iron-component-page.html')

    +live-reload

  body
    iron-component-page(src='" name ".html')"))

        ;; jade
        (f-write-file jade (concat "include ../../global/global.jade

+import('../x-base/x-base.html')

include:comment README.md

+dom-module('" name "')
  p " name))

        ;; styl
        (f-write-file styl (concat "@import '../../global/global.styl'

" name "
  layout-vertical()
"))

        ;; ls
        (f-write-file ls (concat "Polymer do
  is: '" name "'

  behaviors:
    * Polymer.BaseBehavior
    ...

  properties: { }
"))
        ;; bower
        (f-write-file bower (concat "{
  \"name\": \"" name "\",
  \"version\": \"0.0.0\",
  \"description\": \"" name "\",
  \"authors\": [
    \"curimit\"
  ],
  \"keywords\": [
    \"curimit\"
  ],
  \"main\": \"" name ".html\",
  \"private\": true,
  \"license\": \"http://polymer.github.io/LICENSE.txt\",
  \"homepage\": \"http://curimit.com/blog\",
  \"dependencies\": {
    \"polymer\": \"Polymer/polymer#^1.0.0\"
  },
  \"devDependencies\": {
    \"iron-component-page\": \"polymerelements/iron-component-page#latest\",
    \"test-fixture\": \"polymerelements/test-fixture#latest\",
    \"web-component-tester\": \"*\",
    \"webcomponentsjs\": \"webcomponents/webcomponentsjs#latest\"
  }
}
"))

        ;; readme
        (f-write-file readme (concat name "
============

This is the document of `" name "`.

Example:
```html
<" name "></" name ">
```

@demo demo/index.html"))

        ;; demo
        (f-write-file demo (concat "include ../../../global/global.jade

html
  head
    meta(charset='utf-8')
    meta(http-equiv='X-UA-Compatible' content='IE=edge,chrome=1')
    meta(name='viewport' content='width=device-width, minimum-scale=1.0, initial-scale=1, user-scalable=yes')

    title " name " demo

    +js('../../webcomponentsjs/webcomponents-lite.js')
    +import('../" name ".html')

    +live-reload

  body(class='vertical layout')
    " name "(class='flex')"))

        ;; test-index
        (f-write-file test-index (concat "include ../../../global/global.jade

html
  head
    meta(charset='utf-8')
    +js('../../webcomponentsjs/webcomponents-lite.js')
    +js('../../web-component-tester/browser.js')

  body
    +js('test.js')"))

        ;; test-ls
        (f-write-file test-ls (concat "WCT.load-suites []"))

        (find-file jade)
        )
      )
    )
  )

(defun import ()
  (interactive)
  (cond
   ((eq major-mode 'jade-mode)
    (let ((symbol (helm-comp-read "import: " (--map (f-filename it) (f-directories "../")))))
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
