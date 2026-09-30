;;; my/snippets/config.el -*- lexical-binding: t; -*-

(defun +snippets--indent-of (line)
  (string-match "[^ \t]" line))

(defun +snippets--min-indent (lines)
  (when-let* ((indents (delq nil (mapcar #'+snippets--indent-of lines))))
    (apply #'min indents)))

(defun +snippets--reindent-line (line from to)
  (if (>= (length line) from)
      (concat (make-string to ?\s) (substring line from))
    line))

(defun +snippets--reindented-selected-text (indent)
  (when-let* ((text (yas-selected-text))
              (lines (split-string text "\n"))
              (min-indent (+snippets--min-indent lines)))
    (mapconcat (lambda (line) (+snippets--reindent-line line min-indent indent))
               lines "\n")))

(defun +snippets-body (indent)
  "Selected text re-indented to INDENT columns, or an empty INDENT-wide line."
  (or (+snippets--reindented-selected-text indent)
      (make-string indent ?\s)))

(defun +snippets/wrap-region ()
  (interactive)
  (when (region-active-p)
    (let ((beg (save-excursion (goto-char (region-beginning)) (line-beginning-position)))
          (end (save-excursion (goto-char (region-end))       (line-end-position))))
      (set-mark beg)
      (goto-char end)))
  (yas-insert-snippet))
