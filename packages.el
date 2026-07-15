;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;;; Built-in module overrides
(unpin! clj-refactor)
(unpin! flycheck)
(unpin! parinfer-rust-mode)
(unpin! transient)

(disable-packages!
 anaconda-mode
 company-anaconda
 lsp-python-ms       ; prefer lsp-pyright
 nose                ; prefer pytest
 pipenv)             ; prefer poetry

(package! json-mode :disable t)

;;; Editing
(package! key-chord)
(package! drag-stuff)
(package! expand-region)
(package! demo-it)
(package! frog-jump-buffer)
(package! ultra-scroll :recipe (:host github :repo "jdtsmith/ultra-scroll"))
(package! mini-ontop  :recipe (:host github :repo "hkjels/mini-ontop.el"))
(package! topsy       :recipe (:host github :repo "alphapapa/topsy.el"))

;;; UI
(package! beacon)
(package! zoom)

;;; Navigation / search
(package! deadgrep)
(package! cape)

;;; Version control
(package! magit-todos)
(package! magit-delta)
(package! git-link)
(package! why-this)

;;; Languages
(package! clojure-essential-ref-nov)
(package! forth-mode)
(package! po-mode)
(package! sicp)

;;; Python
(package! pet)
(package! python-coverage :recipe (:host github :repo "wbolster/emacs-python-coverage"))
(package! cov)

;;; Data
(package! jsonian      :recipe (:host github :repo "iwahbe/jsonian"))
(package! ob-duckdb    :recipe (:host github :repo "gggion/ob-duckdb" :files ("*.el")))
(package! d2-mode)
(package! plz.el       :recipe (:host github :repo "vlnn/plz.el"))
(package! emacsql :pin "491105a")

;;; Org / notes / planning
(package! denote)
(package! denote-projectile-notes :recipe (:local-repo "~/src/emacs/denote-projectile-notes"))
(package! org-shortcut    :recipe (:host github :repo "glittershark/org-clubhouse"))
(package! org-static-blog :recipe (:host github :repo "bastibe/org-static-blog"))
(package! org-trello :recipe (:build (:not native-compile)))
(package! org-alert)
(package! literate-calc-mode)
(package! hammy :recipe (:host github :repo "alphapapa/hammy.el"))

;;; Reading
(package! elfeed-score)

;;; AI / coding assistants
(package! aider :recipe (:host github :repo "tninja/aider.el"))
(package! ai-code)
(package! mindstream :recipe (:host github :repo "countvajhula/mindstream"))

;;; Tools
(package! github-explorer :recipe (:host github :repo "TxGVNN/github-explorer"))
(package! impatient-mode)
(package! super-save)
(package! uv :recipe (:host github :repo "johannes-mueller/uv.el"))
(package! vterm)

;;; Casual transient menus
(package! casual      :recipe (:host github :repo "kickingvegas/casual"))
(package! casual-avy  :recipe (:host github :repo "kickingvegas/casual-avy"))

;;; Theming experiments
(package! stimmung-themes)
(package! elquery)
(package! mason)


(package! eat
  :recipe (:host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))
