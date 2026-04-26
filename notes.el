;;; notes.el -*- lexical-binding: t; -*-

(use-package! denote
  :config
  (setq denote-directory       (expand-file-name "~/org/denote/")
        denote-known-keywords  '("emacs" "journal" "project" "idea")
        denote-infer-keywords  t
        denote-sort-keywords   t
        denote-file-type       nil ; org by default
        denote-prompts         '(title keywords)))

(use-package! denote-projectile-notes
  :after (denote projectile)
  :config
  (map! :leader
        :prefix "p"
        :desc "Project notes"     "n" #'denote-project-notes
        :desc "New project note"  "N" #'denote-create-project-note))
