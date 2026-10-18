;;; my/ai/flymake.el -*- lexical-binding: t; -*-
;; SPC o l r: have an LLM review every code buffer through Flymake, on open and on save.
;; SPC o l b: the same for this buffer only.
;; SPC o l R: switch the reviewer between llama-server (default) and Claude.
;; SPC o l m: show findings in a column past the code (default), the right margin, or after each line.

(require 'cl-lib)
(require 'flymake)

(declare-function gptel-request "gptel")
(declare-function gptel-get-backend "gptel")
(declare-function gptel-make-anthropic "gptel-anthropic")
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-use-context)
(defvar gptel-use-tools)
(defvar flymake-show-diagnostics-at-end-of-line)

(defconst +ai-flymake-directive
  "You review source code, each line prefixed by its number and a colon. Report what a careful reviewer would say: bugs and unhandled cases as error, non-idiomatic constructs, oversized functions, misleading names, missing or weak tests and asserts lacking explanation strings as warning, remarks worth knowing as note, and possible improvements, simplifications or more idiomatic alternatives as idea. Output one line per finding of the form `N: SEVERITY: message`, N being the line number, SEVERITY one of error, warning, note or idea, the message under 60 characters. Output nothing else: no code, no prose, no fences. Output nothing at all when there is nothing worth saying.")

(defvar +ai-flymake-provider 'llama
  "Who reviews: `llama' for the local llama-server (the default) or `claude'.")

(defvar +ai-flymake-claude-model 'claude-sonnet-4-6)

(defvar +ai-flymake-max-lines 400
  "Buffers longer than this are not sent for review.")

(defvar +ai-flymake-display 'column
  "Where findings show: `column', aligned just past the code; `margin', the window's
right margin; or `eol', right after each line.")

(defvar +ai-flymake-column nil
  "Column the `column' display aligns to; nil means two past the longest line or `fill-column'.")

(defvar +ai-flymake-margin-width 48)

(defvar-local +ai--flymake-run 0
  "Reviews started in this buffer; a reply to an earlier one is dropped.")

(defvar-local +ai--flymake-pending nil
  "The reviewer whose reply this buffer is waiting on, or nil.")

;;; ideas: a fourth severity, counted by flymake as a note, shown quietly

(defface +ai-flymake-idea
  '((t :inherit flymake-note :underline nil))
  "Code an idea was offered for.")

(defface +ai-flymake-idea-echo
  '((t :inherit (font-lock-doc-face italic)))
  "An idea, in the margin or echo area.")

(put '+ai-flymake-idea 'flymake-category 'flymake-note)
(put '+ai-flymake-idea 'face '+ai-flymake-idea)
(put '+ai-flymake-idea 'echo-face '+ai-flymake-idea-echo)
(put '+ai-flymake-idea 'eol-face '+ai-flymake-idea-echo)
(put '+ai-flymake-idea 'bitmap 'flymake-note-bitmap)
(put '+ai-flymake-idea 'severity 10)

;;; providers

(defun +ai--anthropic-key ()
  (or (getenv "ANTHROPIC_API_KEY")
      (user-error "ANTHROPIC_API_KEY is not set; see secrets.el.example")))

(defun +ai--flymake-llama ()
  (cons (or (gptel-get-backend "llama-server") (+ai--llama-backend))
        +ai-fast-model))

(defconst +ai--flymake-claude-backend-name "Claude-review"
  "Our own gptel backend, keyed from the environment; Doom's llm module may register a \"Claude\" too.")

(defun +ai--flymake-claude ()
  (cons (or (gptel-get-backend +ai--flymake-claude-backend-name)
            (gptel-make-anthropic +ai--flymake-claude-backend-name :key #'+ai--anthropic-key))
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

(defun +ai--flymake-buffers ()
  (seq-filter (lambda (buffer) (buffer-local-value '+ai-flymake-mode buffer)) (buffer-list)))

(defun +ai--flymake-restart-all ()
  "Review every buffer with the mode on again, even where flymake had given up on the backend."
  (dolist (buffer (+ai--flymake-buffers))
    (with-current-buffer buffer (flymake-start nil t)))
  (length (+ai--flymake-buffers)))

(defun +ai/flymake-toggle-provider ()
  "Switch the reviewer between llama-server and Claude.
Every reviewed buffer is reviewed again; when nothing is reviewed yet, reviewing starts."
  (interactive)
  (setq +ai-flymake-provider (+ai--flymake-next-provider +ai-flymake-provider))
  (if (+ai--flymake-buffers)
      (message "AI review by %s; re-reviewing %d buffer(s)"
               +ai-flymake-provider (+ai--flymake-restart-all))
    (+ai-flymake-global-mode 1)
    (message "AI review by %s: starting in every code buffer" +ai-flymake-provider)))

;;; replies

(defconst +ai--flymake-severities
  '(("error" . :error) ("warning" . :warning) ("note" . :note) ("idea" . +ai-flymake-idea)))

(defconst +ai--flymake-finding-regexp
  "\\`\\s-*\\([0-9]+\\)[.:)]\\s-*\\(?:\\(error\\|warning\\|note\\|idea\\)\\s-*:\\s-*\\)?\\(.+\\)\\'")

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
  "A flymake diagnostic for FINDING, its line and bare message kept in the data slot."
  (pcase-let ((`(,line ,severity ,text) finding))
    (when-let* (((+ai--flymake-in-range-p buffer line))
                (region (flymake-diag-region buffer line)))
      (flymake-make-diagnostic buffer (car region) (cdr region) severity
                               (format "%s: %s" provider text) (cons line text)))))

(defun +ai--flymake-line (diagnostic) (car (flymake-diagnostic-data diagnostic)))
(defun +ai--flymake-message (diagnostic) (cdr (flymake-diagnostic-data diagnostic)))

(defun +ai--flymake-diagnostics (buffer provider response)
  (delq nil (mapcar (lambda (finding) (+ai--flymake-diagnostic buffer provider finding))
                    (+ai--flymake-parse response))))

(defun +ai--flymake-current-run-p (buffer run)
  (and (buffer-live-p buffer)
       (= run (buffer-local-value '+ai--flymake-run buffer))))

(defun +ai--flymake-findings-text (count)
  (format "%d finding%s" count (if (= count 1) "" "s")))

(defun +ai--flymake-outcome-text (provider count)
  (format "AI review by %s: %s" provider
          (if (zerop count) "nothing to flag" (+ai--flymake-findings-text count))))

(defun +ai--flymake-settle (buffer)
  (with-current-buffer buffer
    (setq +ai--flymake-pending nil)))

(defun +ai--flymake-failure-reason (info)
  (let ((details (delq nil (list (plist-get info :status) (plist-get info :error)))))
    (if details (mapconcat (lambda (d) (format "%s" d)) details " ") "no response")))

(defun +ai--flymake-failure-text (provider info)
  (format "AI review by %s failed: %s" provider (+ai--flymake-failure-reason info)))

(defun +ai--flymake-fail (buffer provider report-fn info)
  "End the run empty and say why; a bad reply must not get the backend disabled."
  (+ai--flymake-settle buffer)
  (funcall report-fn nil)
  (let ((text (+ai--flymake-failure-text provider info)))
    (flymake-log :error "%s" text)
    (message "%s" text)))

;;; where findings show: the right margin, or the end of the line

(defconst +ai--flymake-echo-faces
  '((:error . flymake-error-echo) (:warning . flymake-warning-echo)
    (:note . flymake-note-echo) (+ai-flymake-idea . +ai-flymake-idea-echo)))

(defconst +ai--flymake-severity-order '(:error :warning :note +ai-flymake-idea))

(defconst +ai--flymake-displays '(column margin eol))

(defun +ai--flymake-native-eol-p ()
  (boundp 'flymake-show-diagnostics-at-end-of-line))

(defun +ai--flymake-worst (diagnostics)
  (seq-find (lambda (severity)
              (seq-some (lambda (d) (eq severity (flymake-diagnostic-type d))) diagnostics))
            +ai--flymake-severity-order))

(defun +ai--flymake-summary (diagnostics)
  "One line's findings, bare messages joined, in the face of the worst."
  (propertize (mapconcat #'+ai--flymake-message diagnostics " │ ")
              'face (alist-get (+ai--flymake-worst diagnostics) +ai--flymake-echo-faces)))

(defun +ai--flymake-fit (text)
  (truncate-string-to-width text +ai-flymake-margin-width nil nil "…"))

(defun +ai--flymake-by-line (diagnostics)
  "((LINE DIAGNOSTIC...) ...) from the findings' own line numbers."
  (seq-group-by #'+ai--flymake-line diagnostics))

(defun +ai--flymake-line-bounds (line)
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (cons (point) (line-end-position))))

(defun +ai--flymake-longest-line ()
  (let ((longest 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (end-of-line)
        (setq longest (max longest (current-column)))
        (forward-line 1)))
    longest))

(defun +ai--flymake-column ()
  (or +ai-flymake-column (+ 2 (max fill-column (+ai--flymake-longest-line)))))

(defun +ai--flymake-overlay (beg end)
  (let ((overlay (make-overlay beg end nil t nil)))
    (overlay-put overlay '+ai-flymake t)
    overlay))

(defun +ai--flymake-eol-overlay (line diagnostics)
  (let ((eol (cdr (+ai--flymake-line-bounds line))))
    (overlay-put (+ai--flymake-overlay eol eol)
                 'after-string (concat "  ⟵ " (+ai--flymake-summary diagnostics)))))

(defun +ai--flymake-stretch-to (column)
  (propertize " " 'display `(space :align-to ,column)))

(defun +ai--flymake-column-overlay (line diagnostics)
  (let ((eol (cdr (+ai--flymake-line-bounds line))))
    (overlay-put (+ai--flymake-overlay eol eol)
                 'after-string (concat (+ai--flymake-stretch-to (+ai--flymake-column))
                                       " " (+ai--flymake-summary diagnostics)))))

(defun +ai--flymake-margin-string (diagnostics)
  (propertize " " 'display `((margin right-margin) ,(+ai--flymake-fit (+ai--flymake-summary diagnostics)))))

(defun +ai--flymake-margin-overlay (line diagnostics)
  (let ((bol (car (+ai--flymake-line-bounds line))))
    (overlay-put (+ai--flymake-overlay bol bol)
                 'before-string (+ai--flymake-margin-string diagnostics))))

(defun +ai--flymake-clear ()
  (remove-overlays (point-min) (point-max) '+ai-flymake t))

(defun +ai--flymake-renderer ()
  (pcase +ai-flymake-display
    ('column #'+ai--flymake-column-overlay)
    ('margin #'+ai--flymake-margin-overlay)
    ('eol (unless (+ai--flymake-native-eol-p) #'+ai--flymake-eol-overlay))
    (_ (error "Unknown review display %S; expected one of %S" +ai-flymake-display +ai--flymake-displays))))

(defun +ai--flymake-render (diagnostics)
  (when-let* ((render (+ai--flymake-renderer)))
    (pcase-dolist (`(,line . ,group) (+ai--flymake-by-line diagnostics))
      (funcall render line group))))

(defun +ai--flymake-annotate (buffer diagnostics)
  (with-current-buffer buffer
    (+ai--flymake-clear)
    (+ai--flymake-render diagnostics)))

(defun +ai--flymake-report (buffer provider report-fn response)
  (let ((diagnostics (+ai--flymake-diagnostics buffer provider response)))
    (funcall report-fn diagnostics)
    (+ai--flymake-annotate buffer diagnostics)
    (message "%s" (+ai--flymake-outcome-text provider (length diagnostics)))))

(defun +ai--flymake-callback (buffer run report-fn provider)
  (lambda (response info)
    (when (+ai--flymake-current-run-p buffer run)
      (pcase response
        ((pred stringp)
         (+ai--flymake-settle buffer)
         (+ai--flymake-report buffer provider report-fn response))
        ('nil (+ai--flymake-fail buffer provider report-fn info))
        ('abort
         (+ai--flymake-settle buffer)
         (funcall report-fn nil))))))

;;; the margin column and flymake's own eol display, per buffer

(defun +ai--flymake-apply-margins ()
  (dolist (window (get-buffer-window-list nil nil t))
    (set-window-margins window (car (window-margins window)) right-margin-width)))

(defun +ai--flymake-setup-display ()
  (pcase +ai-flymake-display
    ('margin (setq right-margin-width +ai-flymake-margin-width)
             (+ai--flymake-apply-margins))
    ('eol (when (+ai--flymake-native-eol-p)
            (setq-local flymake-show-diagnostics-at-end-of-line 'short)))))

(defun +ai--flymake-teardown-display ()
  (kill-local-variable 'right-margin-width)
  (+ai--flymake-apply-margins)
  (kill-local-variable 'flymake-show-diagnostics-at-end-of-line))

(defun +ai--flymake-next-display (current)
  (or (cadr (memq current +ai--flymake-displays)) (car +ai--flymake-displays)))

(defun +ai/flymake-toggle-display ()
  "Cycle findings through the column past the code, the right margin and the end of each line."
  (interactive)
  (setq +ai-flymake-display (+ai--flymake-next-display +ai-flymake-display))
  (message "AI review findings shown in %s" +ai-flymake-display)
  (when (bound-and-true-p +ai-flymake-mode)
    (+ai--flymake-teardown-display)
    (+ai--flymake-setup-display)
    (flymake-start nil t)))

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
    (setq +ai--flymake-pending +ai-flymake-provider)
    (message "AI review by %s…" +ai-flymake-provider)
    (gptel-request (+ai--flymake-numbered-buffer)
      :system +ai-flymake-directive
      :callback (+ai--flymake-callback (current-buffer) run report-fn +ai-flymake-provider))))

(defun +ai--flymake-cannot-start (report-fn err)
  (setq +ai--flymake-pending nil)
  (funcall report-fn nil)
  (let ((text (format "AI review by %s could not start: %s" +ai-flymake-provider (error-message-string err))))
    (flymake-log :error "%s" text)
    (message "%s" text)))

(defun +ai-flymake-backend (report-fn &rest _)
  "Flymake backend: ask `+ai-flymake-provider' to review the buffer.
Whatever goes wrong is reported as an empty run, never thrown: an error here
would make flymake disable the backend for the buffer until the mode restarts."
  (if (+ai--flymake-too-long-p)
      (progn (flymake-log :warning "Over %d lines; AI review skipped" +ai-flymake-max-lines)
             (funcall report-fn nil))
    (condition-case err
        (+ai--flymake-request report-fn)
      (error (+ai--flymake-cannot-start report-fn err)))))

;;; mode

(defun +ai--flymake-lighter ()
  (if +ai--flymake-pending (format " AIrev[%s…]" +ai--flymake-pending) " AIrev"))

(defun +ai--flymake-status-text (on count pending)
  (format "AI review by %s: %s" +ai-flymake-provider
          (cond ((not on) "off (SPC o l r to review every code buffer)")
                (pending (format "on in %d buffer(s); this one waiting on %s" count pending))
                (t (format "on in %d buffer(s); this one idle" count)))))

(defun +ai/flymake-status ()
  "Say who reviews, where the review is on, and whether this buffer's is still out."
  (interactive)
  (let ((buffers (+ai--flymake-buffers)))
    (message "%s" (+ai--flymake-status-text buffers (length buffers) +ai--flymake-pending))))

(define-minor-mode +ai-flymake-mode
  "Review this buffer with an LLM through Flymake, on save and when the mode starts.
Edits alone do not trigger a review; a model call per keystroke would be noise.
`+ai-flymake-global-mode' turns this on in every file-backed code buffer."
  :lighter (:eval (+ai--flymake-lighter))
  (cond (+ai-flymake-mode
         (add-hook 'flymake-diagnostic-functions #'+ai-flymake-backend nil t)
         (setq-local flymake-no-changes-timeout nil
                     flymake-fringe-indicator-position 'right-fringe)
         (+ai--flymake-setup-display)
         (if flymake-mode (flymake-start) (flymake-mode 1)))
        (t
         (remove-hook 'flymake-diagnostic-functions #'+ai-flymake-backend t)
         (+ai--flymake-clear)
         (+ai--flymake-teardown-display)
         (kill-local-variable 'flymake-no-changes-timeout)
         (kill-local-variable 'flymake-fringe-indicator-position)
         (flymake-mode -1))))

(defun +ai--flymake-reviewable-p ()
  (and buffer-file-name (derived-mode-p 'prog-mode) t))

(defun +ai--flymake-turn-on ()
  (when (+ai--flymake-reviewable-p)
    (+ai-flymake-mode 1)))

(define-globalized-minor-mode +ai-flymake-global-mode +ai-flymake-mode +ai--flymake-turn-on
  :group 'flymake)

(map! :leader
      :desc "AI review code buffers" "o l r" #'+ai-flymake-global-mode
      :desc "AI review this buffer"  "o l b" #'+ai-flymake-mode
      :desc "AI review provider" "o l R" #'+ai/flymake-toggle-provider
      :desc "AI review display"  "o l m" #'+ai/flymake-toggle-display
      :desc "AI review status"   "o l ?" #'+ai/flymake-status
      :desc "AI review list"     "o l l" #'flymake-show-buffer-diagnostics)
