;;; my/ai/flymake.el -*- lexical-binding: t; -*-
;; SPC o l r: have an LLM review the buffer through Flymake, on save and when the mode starts.
;; SPC o l R: switch the reviewer between llama-server (default) and Claude.

(require 'cl-lib)
(require 'flymake)

(declare-function gptel-request "gptel")
(declare-function gptel-get-backend "gptel")
(declare-function gptel-make-anthropic "gptel-anthropic")
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-use-context)
(defvar gptel-use-tools)

(defconst +ai-flymake-directive
  "You review source code, each line prefixed by its number and a colon. Report only what a careful reviewer would act on: bugs, unhandled cases, non-idiomatic constructs, oversized functions, misleading names, missing or weak tests, asserts lacking explanation strings. Output one line per finding of the form `N: SEVERITY: message`, N being the line number, SEVERITY one of error, warning or note, the message under 100 characters. Output nothing else: no code, no prose, no fences. Output nothing at all when there is nothing worth saying.")

(defvar +ai-flymake-provider 'llama
  "Who reviews: `llama' for the local llama-server (the default) or `claude'.")

(defvar +ai-flymake-claude-model 'claude-sonnet-4-6)

(defvar +ai-flymake-max-lines 400
  "Buffers longer than this are not sent for review.")

(defvar-local +ai--flymake-run 0
  "Reviews started in this buffer; a reply to an earlier one is dropped.")

;;; providers

(defun +ai--anthropic-key ()
  (or (getenv "ANTHROPIC_API_KEY")
      (user-error "ANTHROPIC_API_KEY is not set; see secrets.el.example")))

(defun +ai--flymake-llama ()
  (cons (or (gptel-get-backend "llama-server") (+ai--llama-backend))
        +ai-fast-model))

(defun +ai--flymake-claude ()
  (cons (or (gptel-get-backend "Claude")
            (gptel-make-anthropic "Claude" :key #'+ai--anthropic-key))
        +ai-flymake-claude-model))

(defconst +ai-flymake-providers
  '((llama . +ai--flymake-llama)
    (claude . +ai--flymake-claude)))

(defun +ai--flymake-provider (name)
  "The (BACKEND . MODEL) gptel uses for reviewer NAME."
  (funcall (or (alist-get name +ai-flymake-providers)
               (error "Unknown review provider %S; expected one of %S"
                      name (mapcar #'car +ai-flymake-providers)))))

(defun +ai--flymake-next-provider (current)
  (let ((names (mapcar #'car +ai-flymake-providers)))
    (or (cadr (memq current names)) (car names))))

(defun +ai/flymake-toggle-provider ()
  "Switch the reviewer between llama-server and Claude, re-reviewing this buffer if it is on."
  (interactive)
  (setq +ai-flymake-provider (+ai--flymake-next-provider +ai-flymake-provider))
  (message "AI review by %s" +ai-flymake-provider)
  (when (bound-and-true-p +ai-flymake-mode)
    (flymake-start)))

;;; replies

(defconst +ai--flymake-severities
  '(("error" . :error) ("warning" . :warning) ("note" . :note)))

(defconst +ai--flymake-finding-regexp
  "\\`\\s-*\\([0-9]+\\)[.:)]\\s-*\\(?:\\(error\\|warning\\|note\\)\\s-*:\\s-*\\)?\\(.+\\)\\'")

(defun +ai--flymake-severity (word)
  (if word (alist-get (downcase word) +ai--flymake-severities :note nil #'equal) :note))

(defun +ai--flymake-finding (line)
  (let ((case-fold-search t))
    (when (string-match +ai--flymake-finding-regexp line)
      (list (string-to-number (match-string 1 line))
            (+ai--flymake-severity (match-string 2 line))
            (string-trim (match-string 3 line))))))

(defun +ai--flymake-parse (response)
  "((LINE SEVERITY MESSAGE) ...) from RESPONSE, skipping lines that are not findings."
  (delq nil (mapcar #'+ai--flymake-finding (split-string response "\n" t))))

(defun +ai--flymake-in-range-p (buffer line)
  (with-current-buffer buffer
    (<= 1 line (count-lines (point-min) (point-max)))))

(defun +ai--flymake-diagnostic (buffer provider finding)
  (pcase-let ((`(,line ,severity ,text) finding))
    (when-let* (((+ai--flymake-in-range-p buffer line))
                (region (flymake-diag-region buffer line)))
      (flymake-make-diagnostic buffer (car region) (cdr region) severity
                               (format "%s: %s" provider text)))))

(defun +ai--flymake-diagnostics (buffer provider response)
  (delq nil (mapcar (lambda (finding) (+ai--flymake-diagnostic buffer provider finding))
                    (+ai--flymake-parse response))))

(defun +ai--flymake-current-run-p (buffer run)
  (and (buffer-live-p buffer)
       (= run (buffer-local-value '+ai--flymake-run buffer))))

(defun +ai--flymake-callback (buffer run report-fn provider)
  (lambda (response info)
    (when (+ai--flymake-current-run-p buffer run)
      (pcase response
        ((pred stringp)
         (funcall report-fn (+ai--flymake-diagnostics buffer provider response)))
        ('nil
         (funcall report-fn :panic
                  :explanation (format "%s review failed: %s" provider (plist-get info :status))))))))

;;; backend

(defun +ai--flymake-too-long-p ()
  (> (count-lines (point-min) (point-max)) +ai-flymake-max-lines))

(defun +ai--flymake-numbered-buffer ()
  (+ai--annotate-number-lines (buffer-substring-no-properties (point-min) (point-max))))

(defun +ai--flymake-request (report-fn)
  (require 'gptel)
  (let* ((provider (+ai--flymake-provider +ai-flymake-provider))
         (gptel-backend (car provider))
         (gptel-model (cdr provider))
         (gptel-use-context nil)
         (gptel-use-tools nil)
         (run (cl-incf +ai--flymake-run)))
    (gptel-request (+ai--flymake-numbered-buffer)
      :system +ai-flymake-directive
      :callback (+ai--flymake-callback (current-buffer) run report-fn +ai-flymake-provider))))

(defun +ai-flymake-backend (report-fn &rest _)
  "Flymake backend: ask `+ai-flymake-provider' to review the buffer."
  (if (+ai--flymake-too-long-p)
      (progn (flymake-log :warning "Over %d lines; AI review skipped" +ai-flymake-max-lines)
             (funcall report-fn nil))
    (+ai--flymake-request report-fn)))

;;; mode

(define-minor-mode +ai-flymake-mode
  "Review this buffer with an LLM through Flymake, on save and when the mode starts.
Edits alone do not trigger a review; a model call per keystroke would be noise."
  :lighter " AIrev"
  (cond (+ai-flymake-mode
         (add-hook 'flymake-diagnostic-functions #'+ai-flymake-backend nil t)
         (setq-local flymake-no-changes-timeout nil
                     flymake-fringe-indicator-position 'right-fringe)
         (if flymake-mode (flymake-start) (flymake-mode 1)))
        (t
         (remove-hook 'flymake-diagnostic-functions #'+ai-flymake-backend t)
         (kill-local-variable 'flymake-no-changes-timeout)
         (kill-local-variable 'flymake-fringe-indicator-position)
         (flymake-mode -1))))

(map! :leader
      :desc "AI review buffer"   "o l r" #'+ai-flymake-mode
      :desc "AI review provider" "o l R" #'+ai/flymake-toggle-provider)
