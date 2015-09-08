(require 'helm)

(defun f-write-file (name text)
  (f-write-text text 'utf-8 name))

(defun f-read-one-ui-file (path)
  (f-read-text (concat "~/.emacs.d/packages/one-ui/one-sample/" path) 'utf-8))

(defun f-read-one-ui-template (path name)
  (s-replace
   "one-sample" name
   (f-read-one-ui-file path)))

(defun f-output-one-ui-template (path name)
  (f-write-file
   (concat name "/" (s-replace "one-sample" name path))
   (f-read-one-ui-template path name)))

(defun one-ui-git-init (name)
  (magit-run-git-async "init")
  (magit-run-git-async "remote" "add" "origin" (concat "ssh://git@git.wenjies.com/oneui/" name ".git"))
  (magit-run-git-async "add" ".gitignore")
  (magit-run-git-async "commit" "-m" "First commit.")
  (magit-run-git-async "branch" "develop")
  (magit-run-git-async "checkout" "develop"))

(defun one-ui-new ()
  (interactive)
  (let ((name (read-input "Component name: ")))
    (progn
      (f-mkdir name)
      (f-mkdir (concat name "/src"))
      (f-mkdir (concat name "/src/demo"))
      (f-mkdir (concat name "/src/test"))
      (f-output-one-ui-template ".gitignore" name)
      (f-output-one-ui-template "src/README.md" name)
      (f-output-one-ui-template "src/blueprint.json.ls" name)
      (f-output-one-ui-template "src/index.jade" name)
      (f-output-one-ui-template "src/one-sample.jade" name)
      (f-output-one-ui-template "src/one-sample.ls" name)
      (f-output-one-ui-template "src/one-sample.styl" name)
      (f-output-one-ui-template "src/layout.jade" name)
      (f-output-one-ui-template "src/demo/index.jade" name)
      (f-output-one-ui-template "src/test/index.jade" name)
      (f-output-one-ui-template "src/test/test.ls" name)
      (find-file name)
      (one-ui-git-init name)
      (find-file (concat "src/" name ".jade"))
      )))

(defun import ()
  (interactive)
  (cond
   ((eq major-mode 'jade-mode)
    (let ((symbol (helm-comp-read "import: " (--map (f-filename it) (f-directories "../../")))))
      (save-excursion
        (end-of-buffer)
        (re-search-backward "+import(")
        (end-of-line)
        (newline-and-indent)
        (if (eq (s-index-of "/" symbol) nil)
            (progn
              (insert "+import('../")
              (insert symbol)
              (insert "/")
              (insert symbol)
              (insert ".html')")
              )
          (progn
              (insert "+import('../")
              (insert symbol)
              (insert ".html')")
              ))
        )))))

(defun switch-component ()
  (interactive)
  (let ((component (helm-comp-read "switch-component: " (--filter (f-exists? (concat "../../" it "/src/" it ".jade")) (--map (f-filename it) (f-directories "../.."))))))
    (find-file (concat "../../" component "/src/" component ".jade"))
    (find-file (concat "../../" component "/src/" component ".ls"))
    (find-file (concat "../../" component "/src/" component ".styl"))
    (find-file (concat "../../" component "/src/" component ".jade"))
    )
  )

(provide 'one-ui)
