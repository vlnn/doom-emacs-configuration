;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Everything lives in modules/my/*; see init.el's :my block and README.md.

;; I don't like to comment out a block of lisp with ;
(defmacro comment (&rest _body)
  "Comment out one or more s-expressions."
  nil)
