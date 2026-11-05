;;; my/ai/summarize.el -*- lexical-binding: t; -*-
;; SPC e b: what the region (else the buffer) does, in a popup; C-u uses the reasoning model.
;; A source over `+ai-flymake-max-lines' goes as an outline: top-level lines, signatures, docstrings.
;; The popup is a gptel chat with the source as context, so follow-up questions are typed below.
;; SPC e d: what the defun at point does, in a posframe at its head.

(defvar gptel-context)
(defvar gptel-use-context)
(defvar gptel--system-message)
(defvar gptel-prompt-prefix-alist)
(defvar gptel-quick-system-message)
(defvar gptel-quick-word-count)
(declare-function gptel-mode "gptel")
(declare-function gptel-quick "gptel-quick")

(defvar gptel-model)
(declare-function gptel-request "gptel")
(declare-function markdown-mode "markdown-mode")

(defconst +ai-summarize-directive
  "You explain what a program does. You receive source between <source> tags with its file name. Describe behaviour only: never mention style, naming, structure, bugs, or improvements. Answer in Markdown with exactly these sections: `# Purpose` (one sentence), `# Entry points` (functions or the script body a caller or runtime starts from), `# Inputs → outputs` (arguments, stdin, files, env, return values, exit codes), `# Side effects` (files, network, processes, databases; `none` if none), `# Flow` (three to six bullets in execution order). Under 200 words. No code, no fences.")

(defconst +ai-summarize-followup-directive
  "You answered about a program whose source is in your context. Answer follow-up questions about what it does, pointing at the lines concerned. Behaviour only: never style, bugs or improvements. Be brief.")

(defvar +ai-summarize-model)

(defvar-local +ai--summarize-state 'waiting
  "`waiting' for the first chunk, `streaming' while they arrive, `done' after.")

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
(defun +ai--summarize-chat (source)
  "Make the current buffer a gptel chat whose context is SOURCE, for follow-up questions."
  (when (fboundp 'gptel-mode)
    (setq-local gptel-context (list source)
                gptel-use-context 'system
                gptel--system-message +ai-summarize-followup-directive)
    (gptel-mode 1)))

;;;###autoload
(defun +ai--summarize-buffer (source)
  (with-current-buffer (get-buffer-create "*gptel-summary*")
    (let ((inhibit-read-only t))
      (read-only-mode -1)
      (erase-buffer)
      (unless (derived-mode-p 'markdown-mode)
        (delay-mode-hooks (markdown-mode)))
      (insert "Summarizing...")
      (setq +ai--summarize-state 'waiting)
      (+ai--summarize-chat source)
      (evil-local-set-key 'normal (kbd "q") #'quit-window))
    (current-buffer)))

;;;###autoload
(defun +ai--summarize-append (buffer chunk)
  "Add CHUNK to BUFFER as the model's turn; the first one replaces the placeholder."
  (with-current-buffer buffer
    (when (eq +ai--summarize-state 'waiting)
      (erase-buffer)
      (setq +ai--summarize-state 'streaming))
    (save-excursion
      (goto-char (point-max))
      (insert (propertize chunk 'gptel 'response)))))

;;;###autoload
(defun +ai--summarize-prefix ()
  (alist-get major-mode (bound-and-true-p gptel-prompt-prefix-alist) ""))

;;;###autoload
(defun +ai--summarize-finish (buffer)
  "Close the model's turn in BUFFER and open the next one, ready to type a question."
  (with-current-buffer buffer
    (setq +ai--summarize-state 'done)
    (goto-char (point-max))
    (insert "\n\n" (+ai--summarize-prefix))))

;;;###autoload
(defun +ai--summarize-callback (buffer)
  (lambda (response info)
    (when (buffer-live-p buffer)
      (pcase response
        ((pred stringp) (+ai--summarize-append buffer response))
        ('t (+ai--summarize-finish buffer))
        ('nil (message "Summarize failed: %s" (plist-get info :status)))))))

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
                (buffer (+ai--summarize-buffer (current-buffer))))
      (pop-to-buffer buffer)
      (when outlined (message "Over %d lines; sending an outline" +ai-flymake-max-lines))
      (gptel-request (+ai--summarize-prompt name major-mode source outlined)
        :system +ai-summarize-directive
        :stream t
        :callback (+ai--summarize-callback buffer)))))

;;; the defun at point, in a bubble

;;;###autoload
(defun +ai--defun-directive (count)
  (format "You explain what one function does, in at most %d words: what it takes, what it returns or changes, and the one thing worth knowing about how. Behaviour only: never style, naming, bugs or improvements. No code." count))

;;;###autoload
(defun +ai--defun-at-point ()
  "(TEXT . START) of the defun at point."
  (if-let* ((bounds (bounds-of-thing-at-point 'defun)))
      (cons (string-trim-right (buffer-substring-no-properties (car bounds) (cdr bounds)) "\n")
            (car bounds))
    (user-error "No function at point")))

;;;###autoload
(defun +ai/explain-defun ()
  "Say what the defun at point does, in a bubble at its head."
  (interactive)
  (require 'gptel-quick)
  (pcase-let ((`(,text . ,start) (+ai--defun-at-point))
              (gptel-quick-system-message #'+ai--defun-directive)
              (gptel-quick-word-count 60))
    (save-excursion
      (goto-char start)
      (gptel-quick text))))
