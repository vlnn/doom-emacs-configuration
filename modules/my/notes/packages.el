;; -*- no-byte-compile: t; -*-
(package! denote)
(when (file-directory-p "~/src/emacs/denote-projectile-notes")
  (package! denote-projectile-notes :recipe (:local-repo "~/src/emacs/denote-projectile-notes")))
(package! ob-duckdb :recipe (:host github :repo "gggion/ob-duckdb" :files ("*.el")))
