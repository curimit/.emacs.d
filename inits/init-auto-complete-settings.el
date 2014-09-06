;; auto complete settings

(defun ac-settings-4-cpp ()
  (add-to-list 'ac-omni-completion-sources
               (cons "\\." '(ac-source-semantic)))
  (add-to-list 'ac-omni-completion-sources
               (cons "->" '(ac-source-semantic)))
  (add-to-list 'ac-omni-completion-sources
               (cons "::" '(ac-source-semantic)))
  (setq ac-sources
        '(ac-source-semantic
          ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-all-buffer))
  )

(defun ac-settings-4-java ()
  (add-to-list 'ac-omni-completion-sources
               (cons "\\." '(ac-source-semantic)))
  
  (setq ac-sources
        '(ac-source-semantic
          ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-all-buffer))
  )

(defun ac-settings-4-js2 ()
  (setq ac-sources
        '(ac-source-semantic
          ac-source-tern-completion
          ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-all-buffer))
  )

(add-hook 'c-mode-common-hook 'ac-settings-4-cpp)
(add-hook 'java-mode-hook 'ac-settings-4-java)
(add-hook 'js2-mode-hook 'ac-settings-4-js2)

(provide 'init-auto-complete-settings)
