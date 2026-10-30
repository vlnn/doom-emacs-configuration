;;; test/run.el -*- lexical-binding: t; -*-
;; Usage: emacs --batch -l test/run.el   (or bin/test)
;; Every file under modules/my/ loads, autoload/ ones included, then the suite runs.

(let ((root (file-name-directory (directory-file-name (file-name-directory load-file-name)))))
  (add-to-list 'load-path (expand-file-name "test" root))
  (require 'harness)
  (+test--prepare root)
  (+test--load-modules root #'+test--loadable-p)
  (+test--run root))
