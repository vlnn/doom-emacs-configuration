;;; avy-functions.el -*- lexical-binding: t; -*-

(defun +avy--restore-window ()
  (select-window (cdr (ring-ref avy-ring 0)))
  t)

(defun +avy--bounds-at (pt thing)
  (save-excursion
    (goto-char pt)
    (when (eq thing 'sexp) (sp-backward-up-sexp))
    (bounds-of-thing-at-point thing)))

(defun +avy--on-thing (pt thing fn)
  (cl-destructuring-bind (start . end) (+avy--bounds-at pt thing)
    (funcall fn start end))
  (+avy--restore-window))

(defun +avy--yank-in-place ()
  (save-excursion (yank))
  t)

(defun +avy--wrap-in-comment (pt thing)
  (save-excursion
    (goto-char (car (+avy--bounds-at pt thing)))
    (sp-wrap-with-pair "(")
    (insert "comment ")
    (sp-newline))
  t)

(defun +avy--eval-at (pt eval-fn)
  (save-excursion
    (goto-char pt)
    (sp-up-sexp)
    (funcall eval-fn))
  t)

(defun +avy--lookup-at (pt lookup-fn)
  (save-excursion
    (goto-char pt)
    (call-interactively lookup-fn))
  (+avy--restore-window))

(defun avy-action-kill-whole-sexp (pt)   (+avy--on-thing pt 'sexp  #'kill-region))
(defun avy-action-kill-whole-defun (pt)  (+avy--on-thing pt 'defun #'kill-region))
(defun avy-action-copy-whole-sexp (pt)   (+avy--on-thing pt 'sexp  #'copy-region-as-kill))
(defun avy-action-copy-whole-defun (pt)  (+avy--on-thing pt 'defun #'copy-region-as-kill))

(defun avy-action-teleport-whole-sexp (pt)  (avy-action-kill-whole-sexp pt)  (+avy--yank-in-place))
(defun avy-action-teleport-whole-defun (pt) (avy-action-kill-whole-defun pt) (+avy--yank-in-place))
(defun avy-action-clone-whole-sexp (pt)     (avy-action-copy-whole-sexp pt)  (+avy--yank-in-place))
(defun avy-action-clone-whole-defun (pt)    (avy-action-copy-whole-defun pt) (+avy--yank-in-place))

(defun avy-action-comment-whole-sexp (pt) (+avy--wrap-in-comment pt 'sexp))
(defun avy-action-comment-whole-defn (pt) (+avy--wrap-in-comment pt 'defun))

(defun avy-action-clojure-eval-whole-sexp (pt) (+avy--eval-at pt #'cider-eval-last-sexp))
(defun avy-action-clojure-eval-whole-defn (pt) (+avy--eval-at pt #'cider-eval-defun-at-point))

(defun avy-action-lookup-documentation (pt) (+avy--lookup-at pt #'+lookup/documentation))
(defun avy-action-lookup-references (pt)    (+avy--lookup-at pt #'+lookup/references))

(defun avy-action-rename-sexp (pt)
  (avy-action-kill-move pt)
  (evil-insert 1)
  t)

(defun avy-action-rename-whole-sexp (pt)
  (goto-char pt)
  (sp-backward-sexp)
  (sp-kill-sexp)
  (evil-insert 1)
  t)

(defun avy-action-exchange (pt)
  "Exchange sexp at PT with the one at point."
  (set-mark pt)
  (transpose-sexps 0))

(defun avy-action-mark-to-char (pt)
  (activate-mark)
  (goto-char pt))
