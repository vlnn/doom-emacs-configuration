;;; my/ai/trust.el -*- lexical-binding: t; -*-
;; The repo review pi and hax go through before they start (pi.el starts them).

(require 'cl-lib)
(require 'json)

(defconst +ai-trust-pi-entries
  '("settings.json" "mcp.json" "SYSTEM.md" "APPEND_SYSTEM.md"
    "extensions" "skills" "prompts" "themes"))

(defconst +ai-trust-skipped-dirs '("node_modules" "venv" "target" "dist" "build"))

(defconst +ai-trust-context-files '("AGENTS.md" "AGENTS.override.md" "CLAUDE.md")
  "Context files pi loads from a project, matched in any case as a case-insensitive disk serves them.")

(defconst +ai-trust-context-pathspecs
  (mapcar (lambda (name) (concat ":(glob,icase)**/" name)) +ai-trust-context-files))

(defconst +ai-trust-pathspecs
  (append '(".pi" ".agents/skills" ":(icase)justfile" ":(icase).justfile") +ai-trust-context-pathspecs))

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

(defconst +ai-trust--pi-choices
  '((?t "C-c C-c" "trust")
    (?u "C-c C-u" "untrusted once")
    (?d "C-c C-d" "do not trust")
    (?c "C-c C-k" "cancel")))

(defconst +ai-trust--hax-choices
  '((?p "C-c C-c" "proceed")
    (?c "C-c C-k" "cancel")))

;;; files

;;;###autoload
(defun +ai-trust--descend-p (dir)
  (let ((name (file-name-nondirectory dir)))
    (not (or (string-prefix-p "." name) (member name +ai-trust-skipped-dirs)))))

;;;###autoload
(defun +ai-trust--files-under (root relative)
  (let ((path (expand-file-name relative root)))
    (cond ((file-directory-p path) (directory-files-recursively path "" nil))
          ((file-exists-p path) (list path)))))

;;;###autoload
(defun +ai-trust--pi-files (root)
  (mapcan (lambda (entry) (+ai-trust--files-under root (concat ".pi/" entry)))
          +ai-trust-pi-entries))

;;;###autoload
(defun +ai-trust--skipped-path-p (relative)
  (seq-some (lambda (dir) (not (+ai-trust--descend-p dir)))
            (butlast (split-string relative "/"))))

;;;###autoload
(defun +ai-trust--git-context-files (root)
  (mapcar (lambda (relative) (expand-file-name relative root))
          (seq-remove #'+ai-trust--skipped-path-p
                      (apply #'+ai-trust--git root "ls-files" "--cached" "--others" "--exclude-standard"
                             "--" +ai-trust-context-pathspecs))))

;;;###autoload
(defun +ai-trust--context-file-regexp ()
  (concat "\\`" (regexp-opt +ai-trust-context-files) "\\'"))

;;;###autoload
(defun +ai-trust--context-files (root)
  "Every pi context file under ROOT, in any case: the ones git knows about, else all."
  (if (+ai-trust--git-ok-p root "rev-parse" "--is-inside-work-tree")
      (+ai-trust--git-context-files root)
    (let ((case-fold-search t))
      (directory-files-recursively root (+ai-trust--context-file-regexp) nil #'+ai-trust--descend-p))))

;;;###autoload
(defun +ai-trust--scan-files (root)
  "Files under ROOT that reach pi or hax, relative and sorted."
  (sort (mapcar (lambda (file) (file-relative-name file root))
                (append (+ai-trust--pi-files root)
                        (+ai-trust--files-under root ".agents/skills")
                        (+ai-trust--context-files root)
                        (+justfiles root)))
        #'string<))

;;;###autoload
(defun +ai-trust--content-hash (root files)
  (secure-hash 'sha256
               (mapconcat (lambda (file)
                            (concat file "\0" (+read-file (expand-file-name file root))))
                          files "\0")))

;;; flags

;;;###autoload
(defun +ai-trust--line-flags (line)
  (let ((case-fold-search t))
    (mapcar #'car (seq-filter (lambda (pattern) (string-match-p (cdr pattern) line))
                              +ai-trust-patterns))))

;;;###autoload
(defun +ai-trust--flag-lines (text)
  (delq nil (seq-map-indexed (lambda (line i)
                               (when-let* ((labels (+ai-trust--line-flags line)))
                                 (list (1+ i) labels line)))
                             (split-string text "\n"))))

;;;###autoload
(defun +ai-trust--file-flags (root files)
  (seq-filter #'cdr
              (mapcar (lambda (file)
                        (let ((path (expand-file-name file root)))
                          (cons file (and (file-readable-p path)
                                          (+ai-trust--flag-lines (+read-file path))))))
                      files)))

;;; trust.json, in pi's format

;;;###autoload
(defun +ai-trust--valid-entry-p (entry)
  (and (consp entry) (stringp (car entry)) (memq (cdr entry) '(t :false :null))))

;;;###autoload
(defun +ai-trust--parse-store (text)
  "Decisions in TEXT as ((PATH . t|:false|:null) ...), or :invalid."
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

;;;###autoload
(defun +ai-trust--parent-dir (dir)
  (directory-file-name (file-name-directory dir)))

;;;###autoload
(defun +ai-trust--decision-for (store dir)
  "Nearest t or :false in STORE for DIR or one of its parents, like pi."
  (let ((current (directory-file-name dir))
        decision)
    (while (and (not decision) current)
      (setq decision (car (memq (cdr (assoc current store)) '(t :false))))
      (let ((parent (+ai-trust--parent-dir current)))
        (setq current (unless (equal parent current) parent))))
    decision))

;;;###autoload
(defun +ai-trust--store-with (store dir decision)
  (sort (cons (cons dir decision) (assoc-delete-all dir (copy-alist store)))
        (lambda (a b) (string< (car a) (car b)))))

;;;###autoload
(defun +ai-trust--json-value (decision)
  (pcase decision ('t "true") (:false "false") (_ "null")))

;;;###autoload
(defun +ai-trust--serialize-store (store)
  (if (null store)
      "{}\n"
    (concat "{\n"
            (mapconcat (lambda (entry)
                         (format "  %s: %s" (json-encode-string (car entry))
                                 (+ai-trust--json-value (cdr entry))))
                       (sort (copy-alist store) (lambda (a b) (string< (car a) (car b))))
                       ",\n")
            "\n}\n")))

;;;###autoload
(defun +ai-trust--acquire-lock (lock)
  (condition-case nil
      (progn (make-directory lock) t)
    (file-already-exists nil)))

;;;###autoload
(defun +ai-trust--existing-store (file)
  (if (file-exists-p file) (+ai-trust--parse-store (+read-file file)) nil))

;;;###autoload
(defun +ai-trust--replace-file (file text)
  (let ((temp (make-temp-file (expand-file-name ".trust.json.tmp." (file-name-directory file)))))
    (with-temp-file temp (insert text))
    (rename-file temp file t)))

;;;###autoload
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

;;;###autoload
(defun +ai-trust--current-decision (dir)
  (let ((store (+ai-trust--existing-store +ai-trust-store-file)))
    (unless (eq store :invalid)
      (+ai-trust--decision-for store dir))))

;;; reviews and external changes

;;;###autoload
(defun +ai-trust--load-reviews ()
  (when (file-exists-p +ai-trust-reviews-file)
    (ignore-errors (read (+read-file +ai-trust-reviews-file)))))

;;;###autoload
(defun +ai-trust--review-key (agent dir)
  (cons agent dir))

;;;###autoload
(defun +ai-trust--reviews-with (reviews key marker)
  (append (assoc-delete-all key (copy-alist reviews)) (list (cons key marker))))

;;;###autoload
(defun +ai-trust--save-reviews (reviews)
  (make-directory (file-name-directory +ai-trust-reviews-file) t)
  (with-temp-file +ai-trust-reviews-file
    (prin1 reviews (current-buffer))))

;;;###autoload
(defun +ai-trust--external-authors (emails mine)
  (let ((own (mapcar #'downcase mine)))
    (seq-uniq (seq-remove (lambda (email) (member (downcase email) own)) emails))))

;;;###autoload
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

;;;###autoload
(defun +ai-trust--git (root &rest args)
  (apply #'+git-lines "-C" root args))

;;;###autoload
(defun +ai-trust--git-ok-p (root &rest args)
  (apply #'+git-ok-p "-C" root args))

;;;###autoload
(defun +ai-trust--head (root)
  (when (+ai-trust--git-ok-p root "rev-parse" "--verify" "--quiet" "HEAD")
    (car (+ai-trust--git root "rev-parse" "HEAD"))))

;;;###autoload
(defun +ai-trust--authors-since (root sha)
  (apply #'+ai-trust--git root "log" "--format=%ae" (concat sha "..HEAD") "--" +ai-trust-pathspecs))

;;;###autoload
(defun +ai-trust--changed-files (root sha)
  (apply #'+ai-trust--git root "diff" "--name-only" sha "--" +ai-trust-pathspecs))

;;;###autoload
(defun +ai-trust--diff-since (root sha)
  (string-join (apply #'+ai-trust--git root "diff" sha "--" +ai-trust-pathspecs) "\n"))

;;;###autoload
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

;;;###autoload
(defun +ai-trust--marker (root files)
  (if-let* ((head (+ai-trust--head root)))
      (cons 'commit head)
    (cons 'hash (+ai-trust--content-hash root files))))

;;;###autoload
(defun +ai-trust--record-review (key root files)
  (+ai-trust--save-reviews
   (+ai-trust--reviews-with (+ai-trust--load-reviews) key (+ai-trust--marker root files))))

;;; report

;;;###autoload
(defun +ai-trust--flag-line (file flag)
  (pcase-let ((`(,line ,labels ,text) flag))
    (format "%s:%d: [%s] %s" file line (mapconcat #'symbol-name labels ",") (string-trim text))))

;;;###autoload
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

;;;###autoload
(defun +ai-trust--report-buffer-name (root)
  (format "*agent-review:%s*" (file-name-nondirectory (directory-file-name root))))

;;; launch

;;;###autoload
(defun +ai-trust--canonical (root)
  (directory-file-name (file-truename root)))

;;;###autoload
(defun +ai-trust--open-pi-for-trust (root)
  (+ai--eat-in root (format "*pi-trust:%s*" (file-name-nondirectory (directory-file-name root))) '("pi"))
  (message "Could not write %s; run /trust in this pi, quit it, then SPC 3 again" +ai-trust-store-file))

;;;###autoload
(defun +ai-trust--verdict (choice)
  (pcase choice
    (?t 'trusted)
    (?u 'untrusted-once)
    (?d 'distrusted)
    (?p 'proceed)))

;;;###autoload
(defun +ai-trust--pilish-policy (verdict current)
  (if (eq verdict 'untrusted-once) 'no-approve current))

;;;###autoload
(defun +ai-trust--decide (choice key dir root files)
  (pcase choice
    ((or ?u ?p) (+ai-trust--record-review key root files) (+ai-trust--verdict choice))
    ((or ?t ?d)
     (if (+ai-trust--write-decision +ai-trust-store-file dir (if (eq choice ?t) t :false))
         (progn (+ai-trust--record-review key root files) (+ai-trust--verdict choice))
       (+ai-trust--open-pi-for-trust root)
       nil))))

;;;###autoload
(defun +ai-trust--trust-empty (agent dir decision)
  "Trust DIR for pi when it holds nothing to review and no DECISION exists yet.
Reviewing nothing and deciding nothing would leave pi treating the project as
untrusted, so the inputs a task worktree gains, the grilling skill, would never
load. Inputs that reach the main checkout later still bring a review."
  (when (and (eq agent 'pi) (null decision)
             (+ai-trust--write-decision +ai-trust-store-file dir t))
    (message "Nothing for an agent to review in %s; trusted it for pi in %s" dir +ai-trust-store-file)))

;;;###autoload
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
          ((null files) (+ai-trust--record-review key root files)
           (+ai-trust--trust-empty agent dir decision)
           'unchanged)
          (t (list :agent agent :root root :dir dir :key key
                   :reason reason :files files :review review)))))

;;;###autoload
(defun +ai-trust--agent-choices (agent)
  (if (eq agent 'pi) +ai-trust--pi-choices +ai-trust--hax-choices))

;;;###autoload
(defun +ai-trust--choice-hint (agent)
  (mapconcat (pcase-lambda (`(,_ ,key ,label)) (concat key " " label))
             (+ai-trust--agent-choices agent) "  "))

;;;###autoload
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

;;;###autoload
(define-minor-mode +ai-trust-review-mode
  "Decide in an agent review buffer whether the agent may start.")

;;;###autoload
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

;;;###autoload
(defun +ai-trust--gate (agent root on-allow)
  "Call ON-ALLOW with a verdict once AGENT may start in ROOT, after a review if due."
  (pcase (+ai-trust--assess agent root)
    ('unchanged (funcall on-allow 'unchanged))
    (pending (+ai-trust--open-review pending on-allow))))

;;;###autoload
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

;;;###autoload
(defun +ai-trust/accept ()
  "Trust the repo (pi) or proceed (hax), then start the agent."
  (interactive)
  (+ai-trust--choose (car (car (+ai-trust--agent-choices (plist-get +ai-trust--pending :agent))))))

;;;###autoload
(defun +ai-trust/untrusted-once ()
  "Start pi without the repo's .pi/ resources this time."
  (interactive)
  (+ai-trust--choose ?u))

;;;###autoload
(defun +ai-trust/distrust ()
  "Record that pi must not trust this repo, then start it."
  (interactive)
  (+ai-trust--choose ?d))

;;;###autoload
(defun +ai-trust/cancel ()
  "Start nothing."
  (interactive)
  (+ai-trust--choose ?c))