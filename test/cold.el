;;; test/cold.el -*- lexical-binding: t; -*-
;; Usage: emacs --batch -l test/cold.el   (or bin/test --cold)
;; The suite against a cold start: config files load, autoload/ files do not; the
;; loaddefs Emacs generates from the ;;;###autoload cookies stand in for Doom's.
;; Whatever a test reaches that has no cookie, or reads a variable left in an
;; autoload file, fails here.

(require 'loaddefs-gen)

(defun +test--autoload-dirs (root)
  (seq-filter #'file-directory-p
              (directory-files-recursively (expand-file-name "modules" root) "\\`autoload\\'" t)))

(defun +test--generate-loaddefs (root)
  "Write the loaddefs into ROOT, which is on the load path, so their relative file names resolve."
  (let ((file (expand-file-name ".cold-loaddefs.el" root)))
    (loaddefs-generate (+test--autoload-dirs root) file)
    file))

(defun +test--cold-loadable-p (file root)
  (and (not (string-match-p "/autoload/" file))
       (+test--loadable-p file root)))

(let ((root (file-name-directory (directory-file-name (file-name-directory load-file-name)))))
  (add-to-list 'load-path (expand-file-name "test" root))
  (add-to-list 'load-path root)
  (require 'harness)
  (+test--prepare root)
  (+test--load-modules root #'+test--cold-loadable-p)
  (load (+test--generate-loaddefs root) nil t)
  (+test--run root))
