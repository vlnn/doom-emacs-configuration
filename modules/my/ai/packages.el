;; -*- no-byte-compile: t; -*-
(package! aider :recipe (:host github :repo "tninja/aider.el"))
(package! ai-code)
(package! pilish)
(package! markdown-mode)
(package! opencode :recipe (:host codeberg :repo "sczi/opencode.el"))
;; opencode requires markdown-ts-mode outright: built into Emacs 31, ELPA before
(when (< emacs-major-version 31)
  (package! markdown-ts-mode))
(package! vterm)
(package! eat
  :recipe (:host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))
