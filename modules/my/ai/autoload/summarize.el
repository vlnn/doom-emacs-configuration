;;; my/ai/summarize.el -*- lexical-binding: t; -*-
;; SPC o l s: what the region (else the buffer) does, in a popup; C-u uses the reasoning model.
;; A source over `+ai-flymake-max-lines' goes as an outline: top-level lines, signatures, docstrings.

(defvar gptel-model)
(declare-function gptel-request "gptel")
(declare-function markdown-mode "markdown-mode")

(defconst +ai-summarize-directive
  "You explain what a program does. You receive source between <source> tags with its file name. Describe behaviour only: never mention style, naming, structure, bugs, or improvements. Answer in Markdown with exactly these sections: `# Purpose` (one sentence), `# Entry points` (functions or the script body a caller or runtime starts from), `# Inputs → outputs` (arguments, stdin, files, env, return values, exit codes), `# Side effects` (files, network, processes, databases; `none` if none), `# Flow` (three to six bullets in execution order). Under 200 words. No code, no fences.")

(defvar +ai-summarize-model)

(defconst +ai--summarize-signature-regexp
  "\\`\\s-*\\(async\\s-+\\)?\\(def\\|class\\|defn?\\|defmacro\\|function\\|fn\\|func\\)\\_>")

(defconst +ai--summarize-docstring-regexp "\\`\\s-*\\(\"\"\"\\|'''\\|\"\\|'\\|;;;\\)")

;;;###autoload
(defun +ai--summarize-prompt (name mode source &optional outlined)
  (format "File: %s (%s)%s\n<source>\n%s\n</source>"
          name mode (if outlined " — outline only, function bodies omitted" "") source))

;;;###autoload
(defun +ai--summarize-model (think)
  (cond (think +ai-think-model)
        (+ai-summarize-model)
        (t +ai-fast-model)))

;;;###autoload
(defun +ai--summarize-too-long-p (source)
  (> (length (split-string source "\n")) +ai-flymake-max-lines))

;;;###autoload
(defun +ai--summarize-secret-p (name)
  (seq-some (lambda (re) (string-match-p re name)) +ai-flymake-ignore-regexps))

;;;###autoload
(defun +ai--summarize-signature-p (line)
  (string-match-p +ai--summarize-signature-regexp line))

;;;###autoload
(defun +ai--summarize-top-level-p (line)
  (and (not (string-blank-p line)) (not (string-match-p "\\`\\s-" line))))

;;;###autoload
(defun +ai--summarize-outline-line-p (line after-signature)
  (or (+ai--summarize-top-level-p line)
      (+ai--summarize-signature-p line)
      (and after-signature (string-match-p +ai--summarize-docstring-regexp line))))

;;;###autoload
(defun +ai--summarize-outline (source)
  (let (kept after-signature)
    (dolist (line (split-string source "\n"))
      (when (+ai--summarize-outline-line-p line after-signature)
        (push line kept))
      (setq after-signature (and (+ai--summarize-signature-p line) t)))
    (string-join (nreverse kept) "\n")))

;;;###autoload
(defun +ai--summarize-source (source)
  "SOURCE as it is, or outlined when over the cap: (text . outlined-p)."
  (if (not (+ai--summarize-too-long-p source))
      (cons source nil)
    (let ((outline (+ai--summarize-outline source)))
      (if (+ai--summarize-too-long-p outline)
          (user-error "Even the outline is over %d lines; select a region" +ai-flymake-max-lines)
        (cons outline t)))))

;;;###autoload
(defun +ai--summarize-buffer ()
  (with-current-buffer (get-buffer-create "*gptel-summary*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (unless (derived-mode-p 'markdown-mode)
        (delay-mode-hooks (markdown-mode)))
      (insert "Summarizing...")
      (read-only-mode 1)
      (evil-local-set-key 'normal (kbd "q") #'quit-window))
    (current-buffer)))

;;;###autoload
(defun +ai--summarize-show (buffer text)
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (string-trim text)))))

;;;###autoload
(defun +ai--summarize-callback (buffer)
  (lambda (response info)
    (pcase response
      ((pred stringp) (+ai--summarize-show buffer response))
      ('nil (message "Summarize failed: %s" (plist-get info :status))))))

;;;###autoload
(defun +ai/summarize (&optional think)
  "Say what the region (else the buffer) does.
With THINK (\\[universal-argument]) use the reasoning model. A source over
`+ai-flymake-max-lines' goes as an outline of top-level code, signatures and docstrings."
  (interactive "P")
  (require 'gptel)
  (let ((name (file-name-nondirectory (or buffer-file-name (buffer-name)))))
    (when (+ai--summarize-secret-p name)
      (user-error "%s is on the secrets list; not sent" name))
    (pcase-let ((`(,source . ,outlined) (+ai--summarize-source (+ai--implement-context)))
                (gptel-model (+ai--summarize-model think))
                (buffer (+ai--summarize-buffer)))
      (pop-to-buffer buffer)
      (when outlined (message "Over %d lines; sending an outline" +ai-flymake-max-lines))
      (gptel-request (+ai--summarize-prompt name major-mode source outlined)
        :system +ai-summarize-directive
        :callback (+ai--summarize-callback buffer)))))
