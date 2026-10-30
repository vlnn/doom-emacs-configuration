;;; my/ai/pi.el -*- lexical-binding: t; -*-
;; Starting pi and hax: sessions, drafts, model flags, the jail hook on pilish, SPC 3 / SPC 4.
;; The repo review they go through first is trust.el.

(defvar pilish-project-trust-policy)
(defvar pilish-extra-args)
(defvar pilish-executable)

;;; eat buffers

(defun +ai--hax-buffer-name (root)
  (format "*hax:%s*" (file-name-nondirectory (directory-file-name root))))

(defun +ai--live-buffer (name)
  (when-let* ((buffer (get-buffer name)))
    (and (get-buffer-process buffer) buffer)))

(defun +ai--eat-in (root name command)
  (require 'eat)
  (pop-to-buffer (or (+ai--live-buffer name)
                     (let ((default-directory root))
                       (apply #'eat-make (substring name 1 -1) (car command) nil (cdr command))))))

;;; pilish sessions

(defun +ai--insert-draft-if-empty (draft)
  (when (and draft (string-blank-p (buffer-string)))
    (erase-buffer)
    (insert draft)
    t))

(defun +ai--pilish-input-buffer (&optional session)
  (when (fboundp 'pilish--find-session)
    (when-let* ((chat (pilish--find-session (pilish--session-directory) session)))
      (buffer-local-value 'pilish--input-buffer chat))))

(defun +ai--prefill-pilish-input (draft &optional session)
  "Put DRAFT into SESSION's empty input and return that buffer; a busy input gets it on the kill ring."
  (when draft
    (let ((input (+ai--pilish-input-buffer session)))
      (if (and (buffer-live-p input)
               (with-current-buffer input (+ai--insert-draft-if-empty draft)))
          input
        (kill-new draft)
        (message "pi input already has text; draft is on the kill ring: %s" draft)
        nil))))

(defun +ai--send-pilish-input (line &optional session)
  "Send LINE to SESSION's pi when its input is free; pilish queues it while pi is busy."
  (when-let* ((input (+ai--prefill-pilish-input line session)))
    (with-current-buffer input
      (pilish-send))))

(defvar +ai-pi-model-args
  '(("local" "--model" "qwopus-coder")
    ("claude" "--provider" "anthropic" "--model" "claude-sonnet-4-6"))
  "pi CLI flags for each tasks.org :MODEL: value, used when a session starts.")

(defun +ai--pi-model-args (model)
  (cdr (assoc model +ai-pi-model-args)))

(defun +ai--check-pi-model (model)
  "Refuse a tasks.org MODEL that `+ai-pi-model-args' has no flags for, rather than run pi's default."
  (when (and model (not (assoc model +ai-pi-model-args)))
    (user-error "Unknown :MODEL: %s; set one of %s, or add it to `+ai-pi-model-args'"
                model (string-join (mapcar #'car +ai-pi-model-args) ", "))))

(defvar +ai-pi-model-networks '(("local" . "local"))
  "Jail network for each tasks.org :MODEL: value; other models get \"open\".")

(defun +ai--pi-network (model)
  (or (cdr (assoc model +ai-pi-model-networks)) "open"))

(defun +ai--pilish-session (root &optional session)
  (let ((default-directory root))
    (when (and (file-directory-p root) (fboundp 'pilish--find-session))
      (pilish--find-session (pilish--session-directory) session))))

(defun +ai--kill-buffer-quietly (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer (set-buffer-modified-p nil))
    (kill-buffer buffer)))

(defun +ai--signal-group (pid signal)
  (zerop (call-process "kill" nil nil nil (concat "-" signal) "--" (format "-%d" pid))))

(defun +ai--kill-process-tree (process)
  "End PROCESS and everything it started: TERM to its group, then KILL to what remains."
  (when (process-live-p process)
    (let ((pid (process-id process)))
      (set-process-query-on-exit-flag process nil)
      (+ai--signal-group pid "TERM")
      (sleep-for 0.2)
      (+ai--signal-group pid "KILL")
      (delete-process process))))

(defun +ai--stop-pilish-session (root &optional session)
  "End the pi SESSION running in ROOT: its process, chat and input buffers. Nil without one."
  (when-let* ((chat (+ai--pilish-session root session)))
    (let ((process (buffer-local-value 'pilish--process chat))
          (input (buffer-local-value 'pilish--input-buffer chat)))
      (+ai--kill-process-tree process)
      (+ai--kill-buffer-quietly input)
      (+ai--kill-buffer-quietly chat)
      t)))

(defun +ai--live-pilish-session-p (root &optional session)
  (let ((default-directory root))
    (when-let* ((chat (and (file-directory-p root)
                           (fboundp 'pilish--find-session)
                           (pilish--find-session (pilish--session-directory) session))))
      (process-live-p (buffer-local-value 'pilish--process chat)))))

;;; pi processes: jailed at every start, pilish's restarts included

(defvar +ai--pi-next-launch nil
  "How the pi process pilish starts next runs; bound by `+ai--start-pi'.")

(defvar-local +ai--pi-session-launch nil
  "How this chat buffer's pi first started, so /reload and restarts start it the same way.")
(put '+ai--pi-session-launch 'permanent-local t)

(defun +ai--pilish-default (symbol fallback)
  (if (boundp symbol) (default-value symbol) fallback))

(defun +ai--pi-launch-settings (root verdict model)
  "How pi in ROOT runs after review VERDICT on tasks.org MODEL: jail network, flags, trust."
  (list :root root
        :network (+ai--pi-network model)
        :extra-args (append (+ai--pilish-default 'pilish-extra-args nil) (+ai--pi-model-args model))
        :policy (+ai-trust--pilish-policy verdict (+ai--pilish-default 'pilish-project-trust-policy 'default))))

(defun +ai--pi-launch-for (directory)
  "This start's settings: `+ai--start-pi''s, else its session's first, else plain ones for DIRECTORY."
  (or +ai--pi-next-launch
      +ai--pi-session-launch
      (+ai--pi-launch-settings directory nil nil)))

(defun +ai--pi-start-process-jailed (start directory)
  "Around `pilish--start-process': START pi in DIRECTORY jailed, with its session's flags.
The chat buffer keeps them, so pilish's later restarts of the session reuse them."
  (let ((launch (+ai--pi-launch-for directory)))
    (when (derived-mode-p 'pilish-chat-mode)
      (setq-local +ai--pi-session-launch launch))
    (let ((pilish-executable (+ai--jailed (plist-get launch :root)
                                          (+ai--pilish-default 'pilish-executable '("pi"))
                                          (plist-get launch :network)))
          (pilish-extra-args (plist-get launch :extra-args))
          (pilish-project-trust-policy (plist-get launch :policy)))
      (funcall start directory))))

(defun +ai--hook-pi-jail ()
  (advice-add 'pilish--start-process :around #'+ai--pi-start-process-jailed))

(+ai--hook-pi-jail)

(defun +ai--pi-jail-hooked-p ()
  (and (fboundp 'pilish--start-process)
       (advice-member-p #'+ai--pi-start-process-jailed 'pilish--start-process)))

(defun +ai--require-pi-jail (root)
  (when (and (+ai--jail-applies-p root) (not (+ai--pi-jail-hooked-p)))
    (user-error "pilish no longer starts pi through pilish--start-process; not starting pi unjailed")))

(cl-defun +ai--start-pi (root verdict &key draft send prepare session model review-root)
  (ignore review-root)
  (require 'pilish nil t)
  (+ai--require-pi-jail root)
  (when prepare
    (funcall prepare))
  (let ((default-directory root)
        (+ai--pi-next-launch (+ai--pi-launch-settings root verdict model)))
    (pilish session)
    (if send
        (+ai--send-pilish-input draft session)
      (+ai--prefill-pilish-input draft session))))

(cl-defun +ai--launch-pi (root &rest args &key draft send prepare session model review-root)
  "Start or focus pi in ROOT once the repo review allows it.
PREPARE runs after the review and before pi starts; DRAFT fills its empty input,
and is sent at once with SEND. SESSION names a separate session; MODEL picks flags from
`+ai-pi-model-args' for a session that is not running yet. REVIEW-ROOT is the
repo to review, by default the one ROOT belongs to, so task worktrees share
their repo's review; pass it when PREPARE creates ROOT. A running session
skips the review: its inputs are already loaded."
  (ignore draft send prepare)
  (+ai--check-pi-model model)
  (if (+ai--live-pilish-session-p root session)
      (apply #'+ai--start-pi root 'live args)
    (+ai-trust--gate 'pi (or review-root (+main-checkout root))
                     (lambda (verdict) (apply #'+ai--start-pi root verdict args)))))

(defun +ai/pi ()
  "Start or focus this project's pi session, reviewing the repo first when due.
Inside a pi buffer, hide the session instead."
  (interactive)
  (if (derived-mode-p 'pilish-chat-mode 'pilish-input-mode)
      (pilish-toggle)
    (+ai--launch-pi (+project-root))))

(defun +ai/hax ()
  "Start or focus hax in eat at the project root, reviewing the repo first when due."
  (interactive)
  (unless (executable-find "hax")
    (user-error "hax is not on PATH"))
  (let* ((root (+project-root))
         (name (+ai--hax-buffer-name root)))
    (if-let* ((live (+ai--live-buffer name)))
        (pop-to-buffer live)
      (+ai-trust--gate 'hax (+main-checkout root)
                       (lambda (_verdict) (+ai--eat-in root name (+ai--jailed root '("hax"))))))))

(defvar +tasks-agent)
(setq +tasks-agent
      (list :launch #'+ai--launch-pi
            :send #'+ai--send-pilish-input
            :prefill #'+ai--prefill-pilish-input
            :input-buffer #'+ai--pilish-input-buffer
            :stop #'+ai--stop-pilish-session
            :live-p #'+ai--live-pilish-session-p
            :jail #'+ai--jailed
            :check-model #'+ai--check-pi-model
            :models (lambda () (mapcar #'car +ai-pi-model-args))
            :review-model (lambda () +ai-fast-model)))

(set-popup-rule! "^\\*\\(hax\\|pi-trust\\):" :side 'right :size 0.4 :select t :quit nil :ttl nil)
