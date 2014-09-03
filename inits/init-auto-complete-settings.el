;; auto complete settings

(defun ac-settings-4-cpp ()
  (add-to-list 'ac-omni-completion-sources
               (cons "\\." '(ac-source-semantic)))
  (add-to-list 'ac-omni-completion-sources
               (cons "->" '(ac-source-semantic)))
  (setq ac-sources
        '(ac-source-semantic
          ac-source-yasnippet
            ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-all-buffer))
  )

(add-hook 'c-mode-common-hook 'ac-settings-4-cpp)

(provide 'init-auto-complete-settings)
