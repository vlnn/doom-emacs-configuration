;;; avy-functions.el -*- lexical-binding: t; -*-

(defun avy-action-kill-whole-defun (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'defun)
        (kill-region start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

(defun avy-action-kill-whole-sexp (pt)
    (save-excursion
      (goto-char pt)
      (sp-backward-up-sexp)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'sexp)
          (kill-region start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

(defun avy-action-copy-whole-sexp (pt)
  (save-excursion
    (goto-char pt)
    (sp-backward-up-sexp)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'sexp)
        (copy-region-as-kill start end)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun avy-action-copy-whole-defun (pt)
  (save-excursion
    (goto-char pt)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'defun)
      (copy-region-as-kill start end)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun avy-action-teleport-whole-sexp (pt)
  (avy-action-kill-whole-sexp pt)
  (save-excursion (yank)) t)

(defun avy-action-teleport-whole-defun (pt)
  (avy-action-kill-whole-defun pt)
  (save-excursion (yank)) t)

(defun avy-action-clone-whole-sexp (pt)
  (avy-action-copy-whole-sexp pt)
  (save-excursion (yank)) t)

(defun avy-action-clone-whole-defun (pt)
  (avy-action-copy-whole-defun pt)
  (save-excursion (yank)) t)

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

(defun avy-action-comment-whole-sexp (pt)
  (save-excursion
    (progn
     (goto-char pt)
     (sp-backward-up-sexp)
     (evil-insert 1)
     (insert "(comment ")
     (evil-normal-state)))
  t)

(defun avy-action-exchange (pt)
  "Exchange sexp at PT with the one at point."
  (set-mark pt)
  (transpose-sexps 0))

(defun avy-action-mark-to-char (pt)
  (activate-mark)
  (goto-char pt))

(defun avy-action-lookup-documentation (pt)
 (save-excursion
   (goto-char pt)
   (call-interactively '+lookup/documentation))
 (select-window
  (cdr (ring-ref avy-ring 0)))
 t)

(defun avy-action-lookup-references (pt)
  (save-excursion
    (goto-char pt)
    (call-interactively '+lookup/references))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)
