;;; avy.el -*- lexical-binding: t; -*-

(load! "avy-functions.el")

(after! avy
  (setq avy-all-windows t
        avy-single-candidate-jump nil
        avy-timeout-seconds 0.7
        ;; don't use home row for avy mappings — those are for the dispatch alist below
        avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

  (set-face-attribute 'avy-lead-face-0 nil :foreground "black" :background "yellow green")
  (set-face-attribute 'avy-lead-face   nil :foreground "black" :background "lime green")

  (avy-setup-default)

  (setf (alist-get ?x avy-dispatch-alist) 'avy-action-kill-whole-sexp
        (alist-get ?X avy-dispatch-alist) 'avy-action-kill-whole-defun
        (alist-get ?e avy-dispatch-alist) 'avy-action-clojure-eval-whole-sexp
        (alist-get ?E avy-dispatch-alist) 'avy-action-clojure-eval-whole-defn
        (alist-get ?q avy-dispatch-alist) 'avy-action-rename-sexp
        (alist-get ?Q avy-dispatch-alist) 'avy-action-rename-whole-sexp
        (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?w avy-dispatch-alist) 'avy-action-clone-whole-sexp
        (alist-get ?W avy-dispatch-alist) 'avy-action-clone-whole-defun
        (alist-get ?t avy-dispatch-alist) 'avy-action-teleport-whole-sexp
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-defun
        (alist-get ?z avy-dispatch-alist) 'avy-action-zap-to-char
        (alist-get ?m avy-dispatch-alist) 'avy-action-mark-to-char
        (alist-get ?c avy-dispatch-alist) 'avy-action-comment-whole-sexp
        (alist-get ?C avy-dispatch-alist) 'avy-action-comment-whole-defn
        (alist-get ?i avy-dispatch-alist) 'avy-action-lookup-documentation
        (alist-get ?r avy-dispatch-alist) 'avy-action-lookup-references
        (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char))
