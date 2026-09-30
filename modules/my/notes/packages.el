;; -*- no-byte-compile: t; -*-
(package! denote)
(package! denote-projectile-notes :recipe (:local-repo "~/src/emacs/denote-projectile-notes"))
(package! ob-duckdb :recipe (:host github :repo "gggion/ob-duckdb" :files ("*.el")))
