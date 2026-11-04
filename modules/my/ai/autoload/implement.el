;;; my/ai/implement.el -*- lexical-binding: t; -*-
;; SPC o l i: say what to write; the region (else the buffer) is the context, the code lands at point.

(defconst +ai-implement-directive
  "You write code. You receive source between <context> tags, then what to implement. Write only the code asked for, in the language and style of the context and ready to paste at the spot the request names: no prose, no fences, nothing from the context repeated. Prefer small named functions with clear names over one long one; no comments unless the why is non-obvious. In Python tests use pytest, parametrize, mock with pytest-mock, and give every assert an explanation string of the form 'X should Y'.")

(defconst +ai--implement-fence-regexp "^```[^\n]*\n\\(\\(?:.\\|\n\\)*?\\)\n?```")

;;;###autoload
(defun +ai--implement-context ()
  "The region when active, else the whole buffer."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (buffer-substring-no-properties (point-min) (point-max))))

;;;###autoload
(defun +ai--implement-prompt (context request)
  (format "<context>\n%s\n</context>\n\nImplement: %s" context request))

;;;###autoload
(defun +ai--implement-code (response)
  "The code in RESPONSE: what a fence encloses when there is one, else all of it, trimmed."
  (string-trim (if (string-match +ai--implement-fence-regexp response)
                   (match-string 1 response)
                 response)
               "[ \t\n]+" "[ \t\n]+"))

;;;###autoload
(defun +ai--implement-insert (marker code)
  "Put CODE on lines of its own at MARKER, splitting a line it lands inside; the line it starts on."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (unless (bolp) (insert "\n"))
      (prog1 (line-number-at-pos)
        (insert code)
        (unless (eolp) (insert "\n"))
        (unless (bolp) (insert "\n"))))))

;;;###autoload
(defun +ai--implement-lines (code)
  (length (split-string code "\n")))

;;;###autoload
(defun +ai--implement-callback (marker)
  (lambda (response info)
    (when (buffer-live-p (marker-buffer marker))
      (pcase response
        ((pred stringp)
         (let ((code (+ai--implement-code response)))
           (message "Implemented: %d lines at line %d"
                    (+ai--implement-lines code) (+ai--implement-insert marker code))))
        ('nil (message "Implement failed: %s" (plist-get info :status)))))))

;;;###autoload
(defun +ai/implement (request)
  "Ask for REQUEST to be written, the region (else the whole buffer) being the context.
The code lands where point is, on lines of its own; the region stays as it was."
  (interactive (list (read-string "Implement: ")))
  (let ((gptel-model +ai-fast-model)
        (prompt (+ai--implement-prompt (+ai--implement-context) request)))
    (message "Implementing…")
    (gptel-request prompt
      :system +ai-implement-directive
      :callback (+ai--implement-callback (point-marker)))))
