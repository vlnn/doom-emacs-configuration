;;; my/ai/trust.el -*- lexical-binding: t; -*-
;; SPC 3 / SPC 4: review what a repo can feed an agent before pi or hax starts there.

(require 'cl-lib)

(defvar pilish-project-trust-policy)
(defvar pilish-extra-args)
(defvar pilish-executable)

(defvar +ai-trust-store-file (expand-file-name "~/.pi/agent/trust.json"))

(defvar +ai-trust-reviews-file
  (expand-file-name "ai-trust-reviews.el"
                    (if (boundp 'doom-cache-dir) doom-cache-dir user-emacs-directory)))

(defconst +ai-trust-pi-entries
  '("settings.json" "mcp.json" "SYSTEM.md" "APPEND_SYSTEM.md"
    "extensions" "skills" "prompts" "themes"))

(defconst +ai-trust-justfiles '("justfile" "Justfile" ".justfile"))

(defconst +ai-trust-skipped-dirs '("node_modules" "venv" "target" "dist" "build"))

(defconst +ai-trust-pathspecs
  '(".pi" ".agents/skills" "justfile" "Justfile" ".justfile" ":(glob)**/AGENTS.md"))

(defconst +ai-trust-patterns
  `((spawn . ,(rx (or "child_process" "Bun.spawn" "Deno.Command" "execa"
                      (seq bow (or "exec" "execSync" "execFile" "spawn" "spawnSync")
                           (* blank) "("))))
    (network . ,(rx (or (seq bow "fetch" (* blank) "(") "http://" "https://"
                        (seq bow (or "curl" "wget") eow)
                        "XMLHttpRequest" "WebSocket" "net.connect")))
    (pipe-to-shell . ,(rx (or "curl" "wget") (* (not (any "|\n"))) "|" (* blank)
                          (? (or "ba" "z")) "sh" eow))
    (eval . ,(rx (or (seq bow "eval" (* blank) "(") (seq "new Function" (* blank) "("))))
    (base64 . ,(rx (= 80 (any "A-Za-z0-9+/"))))
    (outside-repo . ,(rx (or "~/" "$HOME" "/etc/" "/Users/" "/home/" "../../")))
    (injection . ,(rx (or (seq "ignore" (+ blank) (? "all" (+ blank))
                               (or "previous" "prior" "above") (+ blank) "instructions")
                          (seq "disregard" (+ nonl) "instructions")
                          "system prompt"))))
  "Labelled regexps for lines worth a human look before trusting a repo.")

(defconst +ai-trust--reason-texts
  '((unreviewed . "never reviewed for an agent")
    (undecided . "no pi trust decision for this directory")
    (rewritten . "the reviewed commit is gone (rebase or force-pull)")
    (external . "agent inputs changed by someone else since your review")
    (changed . "agent inputs changed since your review (no git history)")))

(defvar +ai-trust-own-emails nil
  "Commit emails that count as yours besides git's user.email, e.g. other machines.")

(defconst +ai-trust--pi-choices
  '((?t "C-c C-c" "trust")
    (?u "C-c C-u" "untrusted once")
    (?d "C-c C-d" "do not trust")
    (?c "C-c C-k" "cancel")))

(defconst +ai-trust--hax-choices
  '((?p "C-c C-c" "proceed")
    (?c "C-c C-k" "cancel")))

;;; files

(defun +ai-trust--read-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun +ai-trust--descend-p (dir)
  (let ((name (file-name-nondirectory dir)))
    (not (or (string-prefix-p "." name) (member name +ai-trust-skipped-dirs)))))

(defun +ai-trust--files-under (root relative)
  (let ((path (expand-file-name relative root)))
    (cond ((file-directory-p path) (directory-files-recursively path "" nil))
          ((file-exists-p path) (list path)))))

(defun +ai-trust--pi-files (root)
  (mapcan (lambda (entry) (+ai-trust--files-under root (concat ".pi/" entry)))
          +ai-trust-pi-entries))

(defun +ai-trust--skipped-path-p (relative)
  (seq-some (lambda (dir) (not (+ai-trust--descend-p dir)))
            (butlast (split-string relative "/"))))

(defun +ai-trust--git-agents-md-files (root)
  (mapcar (lambda (relative) (expand-file-name relative root))
          (seq-remove #'+ai-trust--skipped-path-p
                      (+ai-trust--git root "ls-files" "--cached" "--others" "--exclude-standard"
                                      "--" ":(glob)**/AGENTS.md"))))

(defun +ai-trust--agents-md-files (root)
  (if (+ai-trust--git-ok-p root "rev-parse" "--is-inside-work-tree")
      (+ai-trust--git-agents-md-files root)
    (directory-files-recursively root "\\`AGENTS\\.md\\'" nil #'+ai-trust--descend-p)))

(defun +ai-trust--justfiles (root)
  (mapcan (lambda (name) (+ai-trust--files-under root name)) +ai-trust-justfiles))

(defun +ai-trust--scan-files (root)
  "Files under ROOT that reach pi or hax, relative and sorted."
  (sort (mapcar (lambda (file) (file-relative-name file root))
                (append (+ai-trust--pi-files root)
                        (+ai-trust--files-under root ".agents/skills")
                        (+ai-trust--agents-md-files root)
                        (+ai-trust--justfiles root)))
        #'string<))

(defun +ai-trust--content-hash (root files)
  (secure-hash 'sha256
               (mapconcat (lambda (file)
                            (concat file "\0" (+ai-trust--read-file (expand-file-name file root))))
                          files "\0")))

;;; flags

(defun +ai-trust--line-flags (line)
  (let ((case-fold-search t))
    (mapcar #'car (seq-filter (lambda (pattern) (string-match-p (cdr pattern) line))
                              +ai-trust-patterns))))

(defun +ai-trust--flag-lines (text)
  (delq nil (seq-map-indexed (lambda (line i)
                               (when-let* ((labels (+ai-trust--line-flags line)))
                                 (list (1+ i) labels line)))
                             (split-string text "\n"))))

(defun +ai-trust--file-flags (root files)
  (seq-filter #'cdr
              (mapcar (lambda (file)
                        (let ((path (expand-file-name file root)))
                          (cons file (and (file-readable-p path)
                                          (+ai-trust--flag-lines (+ai-trust--read-file path))))))
                      files)))

;;; trust.json, in pi's format

(defun +ai-trust--valid-entry-p (entry)
  (and (consp entry) (stringp (car entry)) (memq (cdr entry) '(t :false :null))))

(defun +ai-trust--parse-store (text)
  "Decisions in TEXT as ((PATH . t|:false|:null) ...), or :invalid."
  (require 'json)
  (let ((parsed (condition-case nil
                    (let ((json-object-type 'alist)
                          (json-key-type 'string)
                          (json-array-type 'vector)
                          (json-false :false)
                          (json-null :null))
                      (json-read-from-string text))
                  (error :invalid))))
    (if (and (listp parsed) (seq-every-p #'+ai-trust--valid-entry-p parsed))
        parsed
      :invalid)))

(defun +ai-trust--parent-dir (dir)
  (directory-file-name (file-name-directory dir)))

(defun +ai-trust--decision-for (store dir)
  "Nearest t or :false in STORE for DIR or one of its parents, like pi."
  (let ((current (directory-file-name dir))
        decision)
    (while (and (not decision) current)
      (setq decision (car (memq (cdr (assoc current store)) '(t :false))))
      (let ((parent (+ai-trust--parent-dir current)))
        (setq current (unless (equal parent current) parent))))
    decision))

(defun +ai-trust--store-with (store dir decision)
  (sort (cons (cons dir decision) (assoc-delete-all dir (copy-alist store)))
        (lambda (a b) (string< (car a) (car b)))))

(defun +ai-trust--json-value (decision)
  (pcase decision ('t "true") (:false "false") (_ "null")))

(defun +ai-trust--serialize-store (store)
  (require 'json)
  (if (null store)
      "{}\n"
    (concat "{\n"
            (mapconcat (lambda (entry)
                         (format "  %s: %s" (json-encode-string (car entry))
                                 (+ai-trust--json-value (cdr entry))))
                       (sort (copy-alist store) (lambda (a b) (string< (car a) (car b))))
                       ",\n")
            "\n}\n")))

(defun +ai-trust--acquire-lock (lock)
  (condition-case nil
      (progn (make-directory lock) t)
    (file-already-exists nil)))

(defun +ai-trust--existing-store (file)
  (if (file-exists-p file) (+ai-trust--parse-store (+ai-trust--read-file file)) nil))

(defun +ai-trust--replace-file (file text)
  (let ((temp (make-temp-file (expand-file-name ".trust.json.tmp." (file-name-directory file)))))
    (with-temp-file temp (insert text))
    (rename-file temp file t)))

(defun +ai-trust--write-decision (file dir decision)
  "Set DIR to DECISION in FILE under pi's lock; nil when locked or FILE is not understood."
  (make-directory (file-name-directory file) t)
  (let ((lock (concat file ".lock")))
    (when (+ai-trust--acquire-lock lock)
      (unwind-protect
          (let ((store (+ai-trust--existing-store file)))
            (unless (eq store :invalid)
              (+ai-trust--replace-file file (+ai-trust--serialize-store
                                             (+ai-trust--store-with store dir decision)))
              t))
        (delete-directory lock)))))

(defun +ai-trust--current-decision (dir)
  (let ((store (+ai-trust--existing-store +ai-trust-store-file)))
    (unless (eq store :invalid)
      (+ai-trust--decision-for store dir))))

;;; reviews and external changes

(defun +ai-trust--load-reviews ()
  (when (file-exists-p +ai-trust-reviews-file)
    (ignore-errors (read (+ai-trust--read-file +ai-trust-reviews-file)))))

(defun +ai-trust--review-key (agent dir)
  (cons agent dir))

(defun +ai-trust--reviews-with (reviews key marker)
  (append (assoc-delete-all key (copy-alist reviews)) (list (cons key marker))))

(defun +ai-trust--save-reviews (reviews)
  (make-directory (file-name-directory +ai-trust-reviews-file) t)
  (with-temp-file +ai-trust-reviews-file
    (prin1 reviews (current-buffer))))

(defun +ai-trust--external-authors (emails mine)
  (let ((own (mapcar #'downcase mine)))
    (seq-uniq (seq-remove (lambda (email) (member (downcase email) own)) emails))))

(defun +ai-trust--scan-reason (agent decision review now)
  "Why AGENT needs a review before starting, or nil.
DECISION is pi's trust.json answer, REVIEW the stored marker, NOW the repo state."
  (cond ((null review) 'unreviewed)
        ((and (eq agent 'pi) (null decision)) 'undecided)
        ((eq (car review) 'commit)
         (cond ((not (plist-get now :commit-known)) 'rewritten)
               ((plist-get now :external) 'external)))
        ((not (equal (cdr review) (plist-get now :hash))) 'changed)))

;;; git

(defun +ai-trust--git (root &rest args)
  (apply #'process-lines-ignore-status "git" "-C" root args))

(defun +ai-trust--git-ok-p (root &rest args)
  (zerop (apply #'call-process "git" nil nil nil "-C" root args)))

(defun +ai-trust--head (root)
  (when (+ai-trust--git-ok-p root "rev-parse" "--verify" "--quiet" "HEAD")
    (car (+ai-trust--git root "rev-parse" "HEAD"))))

(defun +ai-trust--authors-since (root sha)
  (apply #'+ai-trust--git root "log" "--format=%ae" (concat sha "..HEAD") "--" +ai-trust-pathspecs))

(defun +ai-trust--changed-files (root sha)
  (apply #'+ai-trust--git root "diff" "--name-only" sha "--" +ai-trust-pathspecs))

(defun +ai-trust--diff-since (root sha)
  (string-join (apply #'+ai-trust--git root "diff" sha "--" +ai-trust-pathspecs) "\n"))

(defun +ai-trust--now (root review files)
  (pcase review
    (`(commit . ,sha)
     (if (+ai-trust--git-ok-p root "cat-file" "-e" (concat sha "^{commit}"))
         (list :commit-known t
               :external (+ai-trust--external-authors
                          (+ai-trust--authors-since root sha)
                          (cons (or (car (+ai-trust--git root "config" "user.email")) "")
                                +ai-trust-own-emails)))
       (list :commit-known nil)))
    (`(hash . ,_) (list :hash (+ai-trust--content-hash root files)))))

(defun +ai-trust--marker (root files)
  (if-let* ((head (+ai-trust--head root)))
      (cons 'commit head)
    (cons 'hash (+ai-trust--content-hash root files))))

(defun +ai-trust--record-review (key root files)
  (+ai-trust--save-reviews
   (+ai-trust--reviews-with (+ai-trust--load-reviews) key (+ai-trust--marker root files))))

;;; report

(defun +ai-trust--flag-line (file flag)
  (pcase-let ((`(,line ,labels ,text) flag))
    (format "%s:%d: [%s] %s" file line (mapconcat #'symbol-name labels ",") (string-trim text))))

(defun +ai-trust--report-text (reason files flags diff)
  (concat (format "Agent review: %s.\n\n" (alist-get reason +ai-trust--reason-texts))
          (mapconcat (lambda (file) (format "%s:1: reaches the agent\n" file)) files "")
          "\n"
          (if flags
              (mapconcat (lambda (entry)
                           (mapconcat (lambda (flag) (concat (+ai-trust--flag-line (car entry) flag) "\n"))
                                      (cdr entry) ""))
                         flags "")
            "No risky lines matched; read the files anyway.\n")
          (if diff (concat "\nDiff since your last review:\n\n" diff "\n") "")))

(defun +ai-trust--report-buffer-name (root)
  (format "*agent-review:%s*" (file-name-nondirectory (directory-file-name root))))

;;; launch

(defun +ai--project-root ()
  (or (projectile-project-root) default-directory))

(defun +ai-trust--canonical (root)
  (directory-file-name (file-truename root)))

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

(defun +ai-trust--open-pi-for-trust (root)
  (+ai--eat-in root (format "*pi-trust:%s*" (file-name-nondirectory (directory-file-name root))) '("pi"))
  (message "Could not write %s; run /trust in this pi, quit it, then SPC 3 again" +ai-trust-store-file))

(defun +ai-trust--verdict (choice)
  (pcase choice
    (?t 'trusted)
    (?u 'untrusted-once)
    (?d 'distrusted)
    (?p 'proceed)))

(defun +ai-trust--pilish-policy (verdict current)
  (if (eq verdict 'untrusted-once) 'no-approve current))

(defun +ai-trust--decide (choice key dir root files)
  (pcase choice
    ((or ?u ?p) (+ai-trust--record-review key root files) (+ai-trust--verdict choice))
    ((or ?t ?d)
     (if (+ai-trust--write-decision +ai-trust-store-file dir (if (eq choice ?t) t :false))
         (progn (+ai-trust--record-review key root files) (+ai-trust--verdict choice))
       (+ai-trust--open-pi-for-trust root)
       nil))))

(defun +ai-trust--assess (agent root)
  "`unchanged' when AGENT may start in ROOT without a review, else the review due."
  (let* ((dir (+ai-trust--canonical root))
         (key (+ai-trust--review-key agent dir))
         (files (+ai-trust--scan-files root))
         (review (cdr (assoc key (+ai-trust--load-reviews))))
         (decision (and (eq agent 'pi) (+ai-trust--current-decision dir)))
         (reason (+ai-trust--scan-reason agent decision review
                                         (+ai-trust--now root review files))))
    (cond ((null reason) 'unchanged)
          ((null files) (+ai-trust--record-review key root files) 'unchanged)
          (t (list :agent agent :root root :dir dir :key key
                   :reason reason :files files :review review)))))

(defun +ai-trust--agent-choices (agent)
  (if (eq agent 'pi) +ai-trust--pi-choices +ai-trust--hax-choices))

(defun +ai-trust--choice-hint (agent)
  (mapconcat (pcase-lambda (`(,_ ,key ,label)) (concat key " " label))
             (+ai-trust--agent-choices agent) "  "))

(defun +ai-trust--pending-report (pending)
  (let* ((root (plist-get pending :root))
         (review (plist-get pending :review))
         (diff-p (eq (plist-get pending :reason) 'external))
         (shown (if diff-p (+ai-trust--changed-files root (cdr review)) (plist-get pending :files))))
    (+ai-trust--report-text (plist-get pending :reason) shown (+ai-trust--file-flags root shown)
                            (and diff-p (+ai-trust--diff-since root (cdr review))))))

(defvar-local +ai-trust--pending nil
  "The review this buffer is waiting on: `+ai-trust--assess' output plus :on-allow.")

(defvar +ai-trust-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'+ai-trust/accept)
    (define-key map (kbd "C-c C-u") #'+ai-trust/untrusted-once)
    (define-key map (kbd "C-c C-d") #'+ai-trust/distrust)
    (define-key map (kbd "C-c C-k") #'+ai-trust/cancel)
    map))

(define-minor-mode +ai-trust-review-mode
  "Decide in an agent review buffer whether the agent may start.")

(defun +ai-trust--open-review (pending on-allow)
  (let ((root (plist-get pending :root))
        (agent (plist-get pending :agent)))
    (with-current-buffer (get-buffer-create (+ai-trust--report-buffer-name root))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (+ai-trust--pending-report pending)))
      (grep-mode)
      (setq default-directory root
            header-line-format (format "Start %s here?  %s" agent (+ai-trust--choice-hint agent)))
      (setq-local +ai-trust--pending (plist-put (copy-sequence pending) :on-allow on-allow))
      (+ai-trust-review-mode 1)
      (pop-to-buffer (current-buffer)))))

(defun +ai-trust--gate (agent root on-allow)
  "Call ON-ALLOW with a verdict once AGENT may start in ROOT, after a review if due."
  (pcase (+ai-trust--assess agent root)
    ('unchanged (funcall on-allow 'unchanged))
    (pending (+ai-trust--open-review pending on-allow))))

(defun +ai-trust--choose (choice)
  (let* ((pending (or +ai-trust--pending (user-error "No agent review waiting here")))
         (agent (plist-get pending :agent)))
    (unless (assq choice (+ai-trust--agent-choices agent))
      (user-error "Not a choice for %s" agent))
    (setq-local +ai-trust--pending nil)
    (quit-window)
    (when-let* ((verdict (and (not (eq choice ?c))
                              (+ai-trust--decide choice (plist-get pending :key) (plist-get pending :dir)
                                                 (plist-get pending :root) (plist-get pending :files)))))
      (with-current-buffer (window-buffer (selected-window))
        (funcall (plist-get pending :on-allow) verdict)))))

(defun +ai-trust/accept ()
  "Trust the repo (pi) or proceed (hax), then start the agent."
  (interactive)
  (+ai-trust--choose (car (car (+ai-trust--agent-choices (plist-get +ai-trust--pending :agent))))))

(defun +ai-trust/untrusted-once ()
  "Start pi without the repo's .pi/ resources this time."
  (interactive)
  (+ai-trust--choose ?u))

(defun +ai-trust/distrust ()
  "Record that pi must not trust this repo, then start it."
  (interactive)
  (+ai-trust--choose ?d))

(defun +ai-trust/cancel ()
  "Start nothing."
  (interactive)
  (+ai-trust--choose ?c))

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
  (when draft
    (let ((input (+ai--pilish-input-buffer session)))
      (unless (and (buffer-live-p input)
                   (with-current-buffer input (+ai--insert-draft-if-empty draft)))
        (kill-new draft)
        (message "pi input already has text; draft is on the kill ring: %s" draft)))))

(defvar +ai-pi-model-args
  '(("local" "--model" "qwopus-coder")
    ("claude" "--provider" "anthropic" "--model" "claude-sonnet-4-6"))
  "pi CLI flags for each tasks.org :MODEL: value, used when a session starts.")

(defun +ai--pi-model-args (model)
  (cdr (assoc model +ai-pi-model-args)))

(defvar +ai-pi-model-networks '(("local" . "local"))
  "Jail network for each tasks.org :MODEL: value; other models get \"open\".")

(defun +ai--pi-network (model)
  (or (cdr (assoc model +ai-pi-model-networks)) "open"))

(defun +ai--live-pilish-session-p (root &optional session)
  (let ((default-directory root))
    (when-let* ((chat (and (fboundp 'pilish--find-session)
                           (pilish--find-session (pilish--session-directory) session))))
      (process-live-p (buffer-local-value 'pilish--process chat)))))

(cl-defun +ai--start-pi (root verdict &key draft prepare session model)
  (when prepare
    (funcall prepare))
  (require 'pilish nil t)
  (let ((default-directory root)
        (pilish-executable (+ai--jailed root (if (boundp 'pilish-executable) pilish-executable '("pi"))
                                        (+ai--pi-network model)))
        (pilish-project-trust-policy
         (+ai-trust--pilish-policy verdict pilish-project-trust-policy))
        (pilish-extra-args (append (bound-and-true-p pilish-extra-args) (+ai--pi-model-args model))))
    (pilish session)
    (+ai--prefill-pilish-input draft session)))

(cl-defun +ai--launch-pi (root &rest args &key draft prepare session model)
  "Start or focus pi in ROOT once the repo review allows it.
PREPARE runs after the review and before pi starts; DRAFT fills its empty input.
SESSION names a separate session; MODEL picks flags from
`+ai-pi-model-args' for a session that is not running yet. A running session
skips the review: its inputs are already loaded."
  (ignore draft prepare model)
  (if (+ai--live-pilish-session-p root session)
      (apply #'+ai--start-pi root 'live args)
    (+ai-trust--gate 'pi root (lambda (verdict) (apply #'+ai--start-pi root verdict args)))))

(defun +ai/pi ()
  "Start or focus this project's pi session, reviewing the repo first when due.
Inside a pi buffer, hide the session instead."
  (interactive)
  (if (derived-mode-p 'pilish-chat-mode 'pilish-input-mode)
      (pilish-toggle)
    (+ai--launch-pi (+ai--project-root))))

(defun +ai/hax ()
  "Start or focus hax in eat at the project root, reviewing the repo first when due."
  (interactive)
  (unless (executable-find "hax")
    (user-error "hax is not on PATH"))
  (let* ((root (+ai--project-root))
         (name (+ai--hax-buffer-name root)))
    (if-let* ((live (+ai--live-buffer name)))
        (pop-to-buffer live)
      (+ai-trust--gate 'hax root (lambda (_verdict) (+ai--eat-in root name (+ai--jailed root '("hax"))))))))

(set-popup-rule! "^\\*agent-review:" :side 'bottom :size 0.4 :select t :quit t)
(set-popup-rule! "^\\*\\(hax\\|pi-trust\\):" :side 'right :size 0.4 :select t :quit nil :ttl nil)
