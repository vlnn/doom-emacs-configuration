;;; my/ai/flymake.el -*- lexical-binding: t; -*-

(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-use-context)
(defvar gptel-use-tools)
(defvar flymake-show-diagnostics-at-end-of-line)
;; SPC o l r: have an LLM review every code buffer through Flymake, on open and on save.
;; SPC o l b: the same for this buffer only.
;; SPC o l R: switch the reviewer between llama-server (default) and Claude.
;; SPC o l m: show findings in a column past the code (default), the right margin, or after each line.
;; SPC o l x / X: switch the review off in this buffer / this project, and back on.

(require 'cl-lib)
(require 'flymake)
(require 'project)

(declare-function gptel-request "gptel")
(declare-function gptel-get-backend "gptel")
(declare-function gptel-make-anthropic "gptel-anthropic")

(defconst +ai-flymake-directive
  "You review source code, each line prefixed by its number and a colon. Report what a careful reviewer would say: bugs and unhandled cases as error, non-idiomatic constructs, oversized functions, misleading names, missing or weak tests and asserts lacking explanation strings as warning, remarks worth knowing as note, and possible improvements, simplifications or more idiomatic alternatives as idea. Output one line per finding of the form `N: SEVERITY: message`, N being the line number, SEVERITY one of error, warning, note or idea, the message under 60 characters. Output nothing else: no code, no prose, no fences. Output nothing at all when there is nothing worth saying.")

(defvar-local +ai--flymake-run 0
  "Reviews started in this buffer; a reply to an earlier one is dropped.")

(defvar-local +ai--flymake-pending nil
  "The reviewer whose reply this buffer is waiting on, or nil.")

;;; ideas: a fourth severity, counted by flymake as a note, shown quietly

;;;###autoload
(defface +ai-flymake-idea
  '((t :inherit flymake-note :underline nil))
  "Code an idea was offered for.")

;;;###autoload
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

;;;###autoload
(defun +ai--anthropic-key ()
  (or (getenv "ANTHROPIC_API_KEY")
      (user-error "ANTHROPIC_API_KEY is not set; see secrets.el.example")))

;;;###autoload
(defun +ai--flymake-known-backend (name)
  "gptel's backend NAME, or nil; gptel's own lookup signals for an unknown name."
  (ignore-errors (gptel-get-backend name)))

;;;###autoload
(defun +ai--flymake-llama ()
  (cons (or (+ai--flymake-known-backend "llama-server") (+ai--llama-backend))
        +ai-fast-model))

(defconst +ai--flymake-claude-backend-name "Claude-review"
  "Our own gptel backend, keyed from the environment; Doom's llm module may register a \"Claude\" too.")

;;;###autoload
(defun +ai--flymake-claude ()
  (require 'gptel-anthropic)
  (cons (or (+ai--flymake-known-backend +ai--flymake-claude-backend-name)
            (gptel-make-anthropic +ai--flymake-claude-backend-name :key #'+ai--anthropic-key))
        +ai-flymake-claude-model))

(defconst +ai-flymake-providers
  '((llama . +ai--flymake-llama)
    (claude . +ai--flymake-claude)))

;;;###autoload
(defun +ai--flymake-provider (name)
  "The (BACKEND . MODEL) gptel uses for reviewer NAME."
  (funcall (or (alist-get name +ai-flymake-providers)
               (error "Unknown review provider %S; expected one of %S"
                      name (mapcar #'car +ai-flymake-providers)))))

;;;###autoload
(defun +ai--flymake-next-provider (current)
  (let ((names (mapcar #'car +ai-flymake-providers)))
    (or (cadr (memq current names)) (car names))))

;;;###autoload
(defun +ai--flymake-buffers ()
  (seq-filter (lambda (buffer) (buffer-local-value '+ai-flymake-mode buffer)) (buffer-list)))

;;;###autoload
(defun +ai--flymake-restart-all ()
  "Review every buffer with the mode on again, even where flymake had given up on the backend."
  (dolist (buffer (+ai--flymake-buffers))
    (with-current-buffer buffer (flymake-start nil t)))
  (length (+ai--flymake-buffers)))

;;;###autoload
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

;;;###autoload
(defun +ai--flymake-severity (word)
  (if word (alist-get (downcase word) +ai--flymake-severities :note nil #'equal) :note))

;;;###autoload
(defun +ai--flymake-finding (line)
  (let ((case-fold-search t))
    (when (string-match +ai--flymake-finding-regexp line)
      (list (string-to-number (match-string 1 line))
            (+ai--flymake-severity (match-string 2 line))
            (string-trim (match-string 3 line))))))

;;;###autoload
(defun +ai--flymake-parse (response)
  "((LINE SEVERITY MESSAGE) ...) from RESPONSE, skipping lines that are not findings."
  (delq nil (mapcar #'+ai--flymake-finding (split-string response "\n" t))))

;;;###autoload
(defun +ai--flymake-in-range-p (buffer line)
  (with-current-buffer buffer
    (<= 1 line (count-lines (point-min) (point-max)))))

;;;###autoload
(defun +ai--flymake-diagnostic (buffer provider finding)
  "A flymake diagnostic for FINDING, its line and bare message kept in the data slot."
  (pcase-let ((`(,line ,severity ,text) finding))
    (when-let* (((+ai--flymake-in-range-p buffer line))
                (region (flymake-diag-region buffer line)))
      (flymake-make-diagnostic buffer (car region) (cdr region) severity
                               (format "%s: %s" provider text) (cons line text)))))

;;;###autoload
(defun +ai--flymake-line (diagnostic) (car (flymake-diagnostic-data diagnostic)))
;;;###autoload
(defun +ai--flymake-message (diagnostic) (cdr (flymake-diagnostic-data diagnostic)))

;;;###autoload
(defun +ai--flymake-diagnostics (buffer provider response)
  (delq nil (mapcar (lambda (finding) (+ai--flymake-diagnostic buffer provider finding))
                    (+ai--flymake-parse response))))

;;;###autoload
(defun +ai--flymake-current-run-p (buffer run)
  (and (buffer-live-p buffer)
       (= run (buffer-local-value '+ai--flymake-run buffer))))

;;;###autoload
(defun +ai--flymake-findings-text (count)
  (format "%d finding%s" count (if (= count 1) "" "s")))

;;;###autoload
(defun +ai--flymake-outcome-text (provider count)
  (format "AI review by %s: %s" provider
          (if (zerop count) "nothing to flag" (+ai--flymake-findings-text count))))

;;;###autoload
(defun +ai--flymake-settle (buffer)
  (with-current-buffer buffer
    (setq +ai--flymake-pending nil)))

;;;###autoload
(defun +ai--flymake-failure-reason (info)
  (let ((details (delq nil (list (plist-get info :status) (plist-get info :error)))))
    (if details (mapconcat (lambda (d) (format "%s" d)) details " ") "no response")))

;;;###autoload
(defun +ai--flymake-failure-text (provider info)
  (format "AI review by %s failed: %s" provider (+ai--flymake-failure-reason info)))

;;;###autoload
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

;;;###autoload
(defun +ai--flymake-native-eol-p ()
  "Whether flymake draws end-of-line findings itself; not while ours must step aside from point."
  (and (boundp 'flymake-show-diagnostics-at-end-of-line)
       (not +ai-flymake-yield)))

;;;###autoload
(defun +ai--flymake-worst (diagnostics)
  (seq-find (lambda (severity)
              (seq-some (lambda (d) (eq severity (flymake-diagnostic-type d))) diagnostics))
            +ai--flymake-severity-order))

;;;###autoload
(defun +ai--flymake-summary (diagnostics)
  "One line's findings, bare messages joined, in the face of the worst."
  (propertize (mapconcat #'+ai--flymake-message diagnostics " │ ")
              'face (alist-get (+ai--flymake-worst diagnostics) +ai--flymake-echo-faces)))

;;;###autoload
(defun +ai--flymake-fit (text)
  (truncate-string-to-width text +ai-flymake-margin-width nil nil "…"))

;;;###autoload
(defun +ai--flymake-group (pairs)
  "((LINE DIAGNOSTIC...) ...) from PAIRS of (LINE . DIAGNOSTIC)."
  (mapcar (lambda (group) (cons (car group) (mapcar #'cdr (cdr group))))
          (seq-group-by #'car pairs)))

;;;###autoload
(defun +ai--flymake-by-line (diagnostics)
  "((LINE DIAGNOSTIC...) ...) from the findings' own line numbers."
  (+ai--flymake-group (mapcar (lambda (d) (cons (+ai--flymake-line d) d)) diagnostics)))

;;; stepping aside: the line point is on keeps its end clear

;;;###autoload
(defun +ai--flymake-rows-around (line total)
  "Lines by distance from LINE, the one below before the one above, within 1..TOTAL."
  (seq-filter (lambda (row) (<= 1 row total))
              (cl-loop for d from 1 to total append (list (+ line d) (- line d)))))

;;;###autoload
(defun +ai--flymake-nearest-free (line taken total)
  (seq-find (lambda (row) (not (memq row taken))) (+ai--flymake-rows-around line total)))

;;;###autoload
(defun +ai--flymake-pointed (diagnostic arrow)
  "A copy of DIAGNOSTIC for display only, its message led by ARROW."
  (flymake-make-diagnostic (flymake-diagnostic-buffer diagnostic)
                           (flymake-diagnostic-beg diagnostic) (flymake-diagnostic-end diagnostic)
                           (flymake-diagnostic-type diagnostic) (flymake-diagnostic-text diagnostic)
                           (cons (+ai--flymake-line diagnostic)
                                 (concat arrow " " (+ai--flymake-message diagnostic)))))

;;;###autoload
(defun +ai--flymake-way-back (from to)
  (if (< from to) "↑" "↓"))

;;;###autoload
(defun +ai--flymake-point-back (group arrow)
  (cons (+ai--flymake-pointed (car group) arrow) (cdr group)))

;;;###autoload
(defun +ai--flymake-yield (groups line total)
  "GROUPS with the one on LINE moved to the nearest free row, its first message pointing back.
With no free row that group is left out; the rest keep their lines."
  (let ((group (alist-get line groups)))
    (if (null group)
        groups
      (let ((others (assq-delete-all line (copy-sequence groups)))
            (row (+ai--flymake-nearest-free line (mapcar #'car groups) total)))
        (if row
            (cons (cons row (+ai--flymake-point-back group (+ai--flymake-way-back line row))) others)
          others)))))

;;;###autoload
(defun +ai--flymake-line-bounds (line)
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (cons (point) (line-end-position))))

;;;###autoload
(defun +ai--flymake-longest-line ()
  (let ((longest 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (end-of-line)
        (setq longest (max longest (current-column)))
        (forward-line 1)))
    longest))

;;;###autoload
(defun +ai--flymake-column ()
  (or +ai-flymake-column (+ 2 (max fill-column (+ai--flymake-longest-line)))))

;;;###autoload
(defun +ai--flymake-overlay (beg end)
  (let ((overlay (make-overlay beg end nil t nil)))
    (overlay-put overlay '+ai-flymake t)
    overlay))

;;;###autoload
(defun +ai--flymake-eol-overlay (line diagnostics)
  (let ((eol (cdr (+ai--flymake-line-bounds line))))
    (overlay-put (+ai--flymake-overlay eol eol)
                 'after-string (concat "  ⟵ " (+ai--flymake-summary diagnostics)))))

;;;###autoload
(defun +ai--flymake-stretch-to (column)
  (propertize " " 'display `(space :align-to ,column)))

;;;###autoload
(defun +ai--flymake-wrap-width (column)
  (or +ai-flymake-wrap-width
      (max 16 (- (window-body-width (or (get-buffer-window) (selected-window))) column 2))))

;;;###autoload
(defun +ai--flymake-wrap (text width)
  (with-temp-buffer
    (insert text)
    (let ((fill-column width))
      (fill-region (point-min) (point-max)))
    (split-string (buffer-string) "\n" t)))

;;;###autoload
(defun +ai--flymake-free-rows (line annotated total)
  "How many lines below LINE, up to TOTAL, have no finding of their own."
  (let ((next (seq-min (cons (1+ total) (seq-filter (lambda (l) (> l line)) annotated)))))
    (- next line 1)))

;;;###autoload
(defun +ai--flymake-cut (line width)
  (concat (truncate-string-to-width line (1- width)) "…"))

;;;###autoload
(defun +ai--flymake-chunks (summary width rows)
  "SUMMARY wrapped to WIDTH over its own row and ROWS below, cut with an ellipsis past that."
  (let* ((face (get-text-property 0 'face summary))
         (lines (+ai--flymake-wrap (substring-no-properties summary) width))
         (kept (seq-take lines (1+ rows)))
         (cut (< (length kept) (length lines))))
    (mapcar (lambda (line) (propertize line 'face face))
            (if cut
                (append (butlast kept) (list (+ai--flymake-cut (car (last kept)) width)))
              kept))))

;;;###autoload
(defun +ai--flymake-column-overlays (line chunks column)
  (seq-map-indexed (lambda (chunk i)
                     (let ((eol (cdr (+ai--flymake-line-bounds (+ line i)))))
                       (overlay-put (+ai--flymake-overlay eol eol)
                                    'after-string (concat (+ai--flymake-stretch-to column) " " chunk))))
                   chunks))

;;;###autoload
(defun +ai--flymake-render-column (groups &optional blocked)
  "Draw GROUPS at the column; no finding wraps onto an annotated line or onto BLOCKED."
  (let* ((column (+ai--flymake-column))
         (width (+ai--flymake-wrap-width column))
         (annotated (delq nil (cons blocked (mapcar #'car groups))))
         (total (count-lines (point-min) (point-max))))
    (pcase-dolist (`(,line . ,diagnostics) groups)
      (+ai--flymake-column-overlays
       line (+ai--flymake-chunks (+ai--flymake-summary diagnostics) width
                                 (+ai--flymake-free-rows line annotated total))
       column))))

;;;###autoload
(defun +ai--flymake-margin-string (diagnostics)
  (propertize " " 'display `((margin right-margin) ,(+ai--flymake-fit (+ai--flymake-summary diagnostics)))))

;;;###autoload
(defun +ai--flymake-margin-overlay (line diagnostics)
  (let ((bol (car (+ai--flymake-line-bounds line))))
    (overlay-put (+ai--flymake-overlay bol bol)
                 'before-string (+ai--flymake-margin-string diagnostics))))

;;;###autoload
(defun +ai--flymake-clear ()
  (remove-overlays (point-min) (point-max) '+ai-flymake t))

;;;###autoload
(defun +ai--flymake-render-each (render groups)
  (pcase-dolist (`(,line . ,group) groups)
    (funcall render line group)))

;;;###autoload
(defun +ai--flymake-yielding-p ()
  "Whether findings step aside from point: never in the margin, which blocks no typing."
  (and +ai-flymake-yield (not (eq +ai-flymake-display 'margin))))

;;;###autoload
(defun +ai--flymake-cleared-line ()
  "The line kept clear of findings, point's while yielding, else nil."
  (when (+ai--flymake-yielding-p) (line-number-at-pos)))

;;;###autoload
(defun +ai--flymake-aside (groups)
  (if-let* ((line (+ai--flymake-cleared-line)))
      (+ai--flymake-yield groups line (count-lines (point-min) (point-max)))
    groups))

;;;###autoload
(defun +ai--flymake-render-groups (groups)
  (let ((groups (+ai--flymake-aside groups)))
    (pcase +ai-flymake-display
      ('column (+ai--flymake-render-column groups (+ai--flymake-cleared-line)))
      ('margin (+ai--flymake-render-each #'+ai--flymake-margin-overlay groups))
      ('eol (unless (+ai--flymake-native-eol-p)
              (+ai--flymake-render-each #'+ai--flymake-eol-overlay groups)))
      (_ (error "Unknown review display %S; expected one of %S" +ai-flymake-display +ai--flymake-displays)))))

;;;###autoload
(defun +ai--flymake-render (diagnostics)
  (+ai--flymake-render-groups (+ai--flymake-by-line diagnostics)))

;;; following the code: findings sit where flymake's underline is now, not where it was

(defvar-local +ai--flymake-stale nil
  "Whether the buffer changed since findings were last placed.")

(defvar-local +ai--flymake-row nil
  "The line point was on when findings were last placed.")

;;;###autoload
(defun +ai--flymake-live-finding (overlay)
  (when-let* ((diagnostic (overlay-get overlay 'flymake-diagnostic))
              ((eq (flymake-diagnostic-backend diagnostic) '+ai-flymake-backend)))
    (cons (line-number-at-pos (overlay-start overlay)) diagnostic)))

;;;###autoload
(defun +ai--flymake-live-findings ()
  "((LINE . DIAGNOSTIC) ...) of our findings, LINE being where flymake's underline sits now."
  (delq nil (mapcar #'+ai--flymake-live-finding (overlays-in (point-min) (point-max)))))

;;;###autoload
(defun +ai--flymake-place (pairs)
  "Draw PAIRS of (LINE . DIAGNOSTIC) afresh and remember the buffer as seen."
  (setq +ai--flymake-stale nil
        +ai--flymake-row (line-number-at-pos))
  (+ai--flymake-clear)
  (+ai--flymake-render-groups (+ai--flymake-group pairs)))

;;;###autoload
(defun +ai--flymake-mark-stale (&rest _)
  (setq +ai--flymake-stale t))

;;;###autoload
(defun +ai--flymake-left-row-p ()
  (not (eql +ai--flymake-row (line-number-at-pos))))

;;;###autoload
(defun +ai--flymake-follow ()
  "After an edit, or a move to another line while yielding, place the findings again."
  (when (or +ai--flymake-stale
            (and (+ai--flymake-yielding-p) (+ai--flymake-left-row-p)))
    (+ai--flymake-place (+ai--flymake-live-findings))))

;;;###autoload
(defun +ai--flymake-annotate (buffer diagnostics)
  (with-current-buffer buffer
    (+ai--flymake-place (mapcar (lambda (d) (cons (+ai--flymake-line d) d)) diagnostics))))

;;;###autoload
(defun +ai--flymake-report (buffer provider report-fn response)
  (let ((diagnostics (+ai--flymake-diagnostics buffer provider response)))
    (funcall report-fn diagnostics)
    (+ai--flymake-annotate buffer diagnostics)
    (message "%s" (+ai--flymake-outcome-text provider (length diagnostics)))))

;;;###autoload
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

;;;###autoload
(defun +ai--flymake-apply-margins ()
  (dolist (window (get-buffer-window-list nil nil t))
    (set-window-margins window (car (window-margins window)) right-margin-width)))

;;;###autoload
(defun +ai--flymake-setup-display ()
  (pcase +ai-flymake-display
    ('margin (setq right-margin-width +ai-flymake-margin-width)
             (+ai--flymake-apply-margins))
    ('eol (when (+ai--flymake-native-eol-p)
            (setq-local flymake-show-diagnostics-at-end-of-line 'short)))))

;;;###autoload
(defun +ai--flymake-teardown-display ()
  (kill-local-variable 'right-margin-width)
  (+ai--flymake-apply-margins)
  (kill-local-variable 'flymake-show-diagnostics-at-end-of-line))

;;;###autoload
(defun +ai--flymake-next-display (current)
  (or (cadr (memq current +ai--flymake-displays)) (car +ai--flymake-displays)))

;;;###autoload
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

;;;###autoload
(defun +ai--flymake-too-long-p ()
  (> (count-lines (point-min) (point-max)) +ai-flymake-max-lines))

;;;###autoload
(defun +ai--flymake-numbered-buffer ()
  (+ai--annotate-number-lines (buffer-substring-no-properties (point-min) (point-max))))

;;;###autoload
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

;;;###autoload
(defun +ai--flymake-cannot-start (report-fn err)
  (setq +ai--flymake-pending nil)
  (funcall report-fn nil)
  (let ((text (format "AI review by %s could not start: %s" +ai-flymake-provider (error-message-string err))))
    (flymake-log :error "%s" text)
    (message "%s" text)))

;;;###autoload
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

;;;###autoload
(defun +ai--flymake-lighter ()
  (if +ai--flymake-pending (format " AIrev[%s…]" +ai--flymake-pending) " AIrev"))

;;;###autoload
(defun +ai--flymake-status-text (on count pending &optional muted)
  (format "AI review by %s: %s" +ai-flymake-provider
          (cond ((not on) "off (SPC o l r to review every code buffer)")
                ((eq muted 'project) (format "on in %d buffer(s); this one off with its project (SPC o l X)" count))
                (muted (format "on in %d buffer(s); this one off (SPC o l x)" count))
                (pending (format "on in %d buffer(s); this one waiting on %s" count pending))
                (t (format "on in %d buffer(s); this one idle" count)))))

;;;###autoload
(defun +ai/flymake-status ()
  "Say who reviews, where the review is on, and whether this buffer's is still out or muted."
  (interactive)
  (let ((buffers (+ai--flymake-buffers)))
    (message "%s" (+ai--flymake-status-text buffers (length buffers) +ai--flymake-pending
                                            (+ai--flymake-muted-how)))))

;;;###autoload
(define-minor-mode +ai-flymake-mode
  "Review this buffer with an LLM through Flymake, on save and when the mode starts.
Edits alone do not trigger a review; a model call per keystroke would be noise.
`+ai-flymake-global-mode' turns this on in every file-backed code buffer."
  :lighter (:eval (+ai--flymake-lighter))
  (cond (+ai-flymake-mode
         (add-hook 'flymake-diagnostic-functions #'+ai-flymake-backend nil t)
         (add-hook 'after-change-functions #'+ai--flymake-mark-stale nil t)
         (add-hook 'post-command-hook #'+ai--flymake-follow nil t)
         (setq-local flymake-no-changes-timeout nil
                     flymake-fringe-indicator-position 'right-fringe)
         (+ai--flymake-setup-display)
         (if flymake-mode (flymake-start) (flymake-mode 1)))
        (t
         (remove-hook 'flymake-diagnostic-functions #'+ai-flymake-backend t)
         (remove-hook 'after-change-functions #'+ai--flymake-mark-stale t)
         (remove-hook 'post-command-hook #'+ai--flymake-follow t)
         (+ai--flymake-clear)
         (+ai--flymake-teardown-display)
         (kill-local-variable 'flymake-no-changes-timeout)
         (kill-local-variable 'flymake-fringe-indicator-position)
         (flymake-mode -1))))

;;;###autoload
(defun +ai--flymake-secret-name-p (file)
  (let ((name (file-name-nondirectory file)))
    (seq-some (lambda (regexp) (string-match-p regexp name)) +ai-flymake-ignore-regexps)))

;;;###autoload
(defun +ai--flymake-git-ignored-p (file)
  (and +ai-flymake-skip-ignored
       (not (file-remote-p file))
       (zerop (call-process "git" nil nil nil "-C" (file-name-directory file) "check-ignore" "-q" file))))

;;;###autoload
(defun +ai--flymake-ignored-p (file)
  "Whether FILE must not reach a reviewer: a secret by name, or ignored by git."
  (and (or (+ai--flymake-secret-name-p file) (+ai--flymake-git-ignored-p file)) t))

;;; switching the review off: this buffer, or its whole project

;;;###autoload
(defvar +ai--flymake-muted-roots nil
  "Project roots `+ai/flymake-mute-project' switched off this session.")

;;;###autoload
(defun +ai--flymake-root ()
  "The project root of this buffer, else its directory."
  (if-let* ((project (project-current nil default-directory)))
      (project-root project)
    default-directory))

;;;###autoload
(defun +ai--flymake-under-p (file root)
  (string-prefix-p (file-name-as-directory (expand-file-name root)) (expand-file-name file)))

;;;###autoload
(defun +ai--flymake-muted-root (file)
  "The muted project root FILE sits in, or nil."
  (seq-find (lambda (root) (+ai--flymake-under-p file root)) +ai--flymake-muted-roots))

;;;###autoload
(defun +ai--flymake-muted-how ()
  "`buffer' when this buffer is muted on its own, `project' through its project, else nil."
  (cond (+ai-flymake-off 'buffer)
        ((and buffer-file-name (+ai--flymake-muted-root buffer-file-name)) 'project)))

;;;###autoload
(defun +ai--flymake-reviewable-p ()
  (and buffer-file-name
       (derived-mode-p 'prog-mode)
       (not (+ai--flymake-ignored-p buffer-file-name))
       (not (+ai--flymake-muted-how))
       t))

;;;###autoload
(defun +ai--flymake-resume ()
  "Review this buffer again if the global mode wants it."
  (when (bound-and-true-p +ai-flymake-global-mode)
    (+ai--flymake-turn-on)))

;;;###autoload
(defun +ai--flymake-silence ()
  (when +ai-flymake-mode
    (+ai-flymake-mode -1)))

;;;###autoload
(defun +ai/flymake-mute-buffer ()
  "Switch AI review off for this buffer, or back on when it was switched off here.
Nothing reviews a muted buffer, not even `+ai-flymake-global-mode' turning on later."
  (interactive)
  (setq-local +ai-flymake-off (not +ai-flymake-off))
  (if +ai-flymake-off
      (progn (+ai--flymake-silence) (message "AI review off in this buffer"))
    (+ai--flymake-resume)
    (message "AI review back on in this buffer")))

;;;###autoload
(defun +ai--flymake-buffers-under (root)
  (seq-filter (lambda (buffer)
                (when-let* ((file (buffer-file-name buffer))) (+ai--flymake-under-p file root)))
              (buffer-list)))

;;;###autoload
(defun +ai--flymake-each-under (root action)
  (dolist (buffer (+ai--flymake-buffers-under root))
    (with-current-buffer buffer (funcall action))))

;;;###autoload
(defun +ai/flymake-mute-project ()
  "Switch AI review off for every buffer of this project, or back on when it was off.
The mute lasts the session; `+ai-flymake-off' in .dir-locals.el makes it permanent."
  (interactive)
  (let ((root (+ai--flymake-root)))
    (if (member root +ai--flymake-muted-roots)
        (progn (setq +ai--flymake-muted-roots (delete root +ai--flymake-muted-roots))
               (+ai--flymake-each-under root #'+ai--flymake-resume)
               (message "AI review back on in %s" root))
      (push root +ai--flymake-muted-roots)
      (+ai--flymake-each-under root #'+ai--flymake-silence)
      (message "AI review off in %s" root))))

;;;###autoload
(defun +ai--flymake-turn-on ()
  (when (+ai--flymake-reviewable-p)
    (+ai-flymake-mode 1)))

;;;###autoload
(define-globalized-minor-mode +ai-flymake-global-mode +ai-flymake-mode +ai--flymake-turn-on
  :group 'flymake)