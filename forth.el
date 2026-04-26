;;; forth.el -*- lexical-binding: t; -*-

(use-package! forth-mode
  :mode ("\\.fs\\'" . forth-mode)
  :config
  (let ((tab forth-mode-syntax-table))
    (dolist (c '(?@ ?! ?? ?> ?< ?+ ?- ?= ?* ?/ ?, ?. ?# ?$ ?% ?& ?'))
      (modify-syntax-entry c "_" tab)))

  (add-hook! 'forth-mode-hook
    (defun +forth/lookup-setup-h ()
      (setq-local +lookup-definition-functions '(+lookup-dumb-jump-backend-fn))
      (setq-local +lookup-references-functions '(+lookup-project-search-backend-fn)))))

(after! dumb-jump
  (add-to-list 'dumb-jump-language-file-exts
               '(:language "forth" :ext "fs" :agtype nil :rgtype nil))
  (add-to-list 'dumb-jump-find-rules
               '(:type "function"
                 :supports ("rg" "grep" "git-grep")
                 :language "forth"
                 :regex ":\\s+JJJ\\s"
                 :tests (": foo dup ;"))))
