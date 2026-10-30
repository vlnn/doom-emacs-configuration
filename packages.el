;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
;; Per-feature packages live next to their config in modules/my/*/packages.el.

(unpin! clj-refactor)
(unpin! flycheck)
(unpin! transient)

(disable-packages!
 anaconda-mode
 company-anaconda
 lsp-python-ms       ; prefer pylsp
 nose                ; prefer pytest
 pipenv)             ; prefer uv

(package! json-mode :disable t)
