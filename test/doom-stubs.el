;;; test/doom-stubs.el -*- lexical-binding: t; -*-
;; Just enough of Doom's macro surface to `load' config files in plain
;; `emacs --batch'. Everything that would touch a live Doom expands to nil.

(defmacro after! (_features &rest _body) nil)
(defmacro map! (&rest _args) nil)
(defmacro use-package! (_name &rest _args) nil)
(defmacro add-hook! (_hooks &rest _body) nil)
(defmacro defadvice! (_symbol _args &rest _body) nil)
(defmacro custom-set-faces! (&rest _specs) nil)
(defmacro load! (_file &rest _args) nil)
(defmacro set-popup-rule! (&rest _args) nil)
(defmacro set-popup-rules! (&rest _args) nil)
(defmacro set-repl-handler! (&rest _args) nil)
(defmacro set-eval-handler! (&rest _args) nil)
(defvar doom-user-dir default-directory)

(provide 'doom-stubs)
