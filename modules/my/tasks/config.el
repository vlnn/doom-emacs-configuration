;;; my/tasks/config.el -*- lexical-binding: t; -*-

(defconst +tasks--dir
  (file-name-directory (or load-file-name (bound-and-true-p byte-compile-current-file) buffer-file-name)))

(defvar +tasks-file-name "tasks.org")

(defvar +tasks-branch-prefixes '("feat/")
  "Git branch prefixes the branches block treats as tracked work.")

(defvar +tasks-agenda-commands '(org-agenda org-agenda-list org-todo-list)
  "Agenda entry points that refresh `org-agenda-files' first.")

(defconst +tasks--skeleton-text
  "#+TODO: IDEA(i) TODO(t) PLAN(p!) RED(r!) GREEN(g!) REVIEW(v!) | DONE(d!) DROPPED(x@)
#+COLUMNS: %45ITEM %8TODO %18BRANCH %7MODEL %12CLOSED
#+PROPERTY: MODEL_ALL local claude
#+ARCHIVE: %s_archive::* Archived

* Ideas
* Active
* Backlog
* Branches
#+BEGIN: branches
#+END:
* Journal
")

(defun +tasks--skeleton ()
  +tasks--skeleton-text)

(defun +tasks--string-list-p (value)
  (and (listp value) (seq-every-p #'stringp value) t))

(put '+tasks-branch-prefixes 'safe-local-variable #'+tasks--string-list-p)

;;; agenda files

(defun +tasks--project-file (root)
  (expand-file-name +tasks-file-name root))

(defun +tasks--project-files (roots)
  (seq-uniq (mapcar #'file-truename
                    (seq-filter #'file-exists-p
                                (mapcar #'+tasks--project-file (seq-remove #'file-remote-p roots))))))

(defun +tasks--agenda-files (base roots)
  (seq-uniq (append base (+tasks--project-files roots))))

(defun +tasks--refresh-agenda-files (&rest _)
  (setq org-agenda-files
        (+tasks--agenda-files (list org-directory) projectile-known-projects)))

(defun +tasks--advise-agenda-commands ()
  (dolist (command +tasks-agenda-commands)
    (advice-add command :before #'+tasks--refresh-agenda-files)))

(defconst +tasks--agenda-custom-commands
  '(("b" "Board" ((todo "PLAN|RED|GREEN|REVIEW" ((org-agenda-overriding-header "In flight")))
                  (todo "TODO" ((org-agenda-overriding-header "Ready")))
                  (todo "IDEA" ((org-agenda-overriding-header "Ideas")))))
    ("w" "Last week" agenda ""
     ((org-agenda-span 7)
      (org-agenda-start-day "-7d")
      (org-agenda-start-with-log-mode '(state closed))))
    ("r" "Needs my review" todo "REVIEW")))

(after! org-agenda
  (+tasks--advise-agenda-commands)
  (setq org-agenda-custom-commands +tasks--agenda-custom-commands))

;;; branches dynamic block

(defun +tasks--git-ok-p (&rest args)
  (zerop (apply #'call-process "git" nil nil nil args)))

(defun +tasks--git-lines (&rest args)
  "Stdout lines of git ARGS, or nil when git fails."
  (with-temp-buffer
    (when (zerop (apply #'call-process "git" nil (list t nil) nil args))
      (split-string (buffer-string) "\n" t))))

(defun +tasks--default-branch-from (origin-head locals)
  (cond (origin-head (string-remove-prefix "refs/remotes/origin/" origin-head))
        ((member "main" locals) "main")
        ((member "master" locals) "master")
        (t "main")))

(defun +tasks--default-branch ()
  (+tasks--default-branch-from
   (car (+tasks--git-lines "symbolic-ref" "-q" "refs/remotes/origin/HEAD"))
   (+tasks--git-lines "branch" "--format=%(refname:short)")))

(defun +tasks--merged-p (branch main)
  (or (+tasks--git-ok-p "merge-base" "--is-ancestor" branch main)
      (+tasks--git-ok-p "merge-base" "--is-ancestor" branch (concat "origin/" main))))

(defun +tasks--ahead (branch main)
  (string-to-number (or (car (+tasks--git-lines "rev-list" "--count" (concat main ".." branch))) "0")))

(defun +tasks--branch-patterns (prefixes)
  (mapcar (lambda (prefix) (concat prefix "*")) prefixes))

(defun +tasks--parse-branch-line (line)
  (pcase-let ((`(,branch ,age) (split-string line "\t")))
    (cons branch age)))

(defun +tasks--git-branches (prefixes)
  (mapcar #'+tasks--parse-branch-line
          (apply #'process-lines "git" "branch" "--list"
                 "--format=%(refname:short)\t%(committerdate:relative)"
                 (+tasks--branch-patterns prefixes))))

(defun +tasks--task-if-on-branch (branch)
  (when-let* ((state (org-get-todo-state)))
    (when (equal branch (org-entry-get nil "BRANCH"))
      (list (org-get-heading t t t t) state (point)))))

(defun +tasks--branch-task (branch)
  "The task linked to BRANCH in the current buffer, as (HEADING STATE POS)."
  (save-restriction
    (widen)
    (car (delq nil (org-map-entries (lambda () (+tasks--task-if-on-branch branch)))))))

(defun +tasks--task-for-branch (branch)
  (car (+tasks--branch-task branch)))

(defun +tasks--state-cell (merged ahead task-state)
  (let ((progress (if merged "merged" (format "ahead %d" ahead))))
    (cond ((not (equal task-state "DONE")) progress)
          (merged "DONE! delete branch")
          (t (concat "DONE! " progress)))))

(defun +tasks--branch-row (branch age task state)
  (format "| %s | %s | %s | %s |" branch age (or task "NONE") state))

(defun +tasks--branch-table (rows)
  (string-join (cons "| Branch | Last commit | Task | State |\n|-" rows) "\n"))

(defun +tasks--branch-row-for (branch age main)
  (pcase-let ((`(,heading ,state ,_) (+tasks--branch-task branch)))
    (+tasks--branch-row branch age heading
                        (+tasks--state-cell (+tasks--merged-p branch main)
                                            (+tasks--ahead branch main)
                                            state))))

(defun +tasks--branch-rows (prefixes)
  (let ((main (+tasks--default-branch)))
    (mapcar (pcase-lambda (`(,branch . ,age)) (+tasks--branch-row-for branch age main))
            (+tasks--git-branches prefixes))))

(defun +tasks--aligned-table (rows)
  (with-temp-buffer
    (insert (+tasks--branch-table rows))
    (delay-mode-hooks (org-mode))
    (goto-char (point-min))
    (org-table-align)
    (string-trim-right (buffer-string))))

(defun +tasks--current-table ()
  (+tasks--aligned-table (+tasks--branch-rows +tasks-branch-prefixes)))

(defun org-dblock-write:branches (_params)
  (insert (+tasks--current-table)))

(defun +tasks--row-branch (line)
  (when (string-match "\\`|[[:blank:]]*\\([^|[:blank:]]+\\)[[:blank:]]*|" line)
    (let ((cell (match-string 1 line)))
      (unless (or (equal cell "Branch") (string-prefix-p "-" cell))
        cell))))

(defun +tasks--block-body-bounds ()
  "Bounds of the branches block body, rows plus their final newline."
  (goto-char (point-min))
  (when (re-search-forward "^[ \t]*#\\+BEGIN: branches.*\n" nil t)
    (let ((start (point)))
      (when (re-search-forward "^[ \t]*#\\+END:" nil t)
        (cons start (match-beginning 0))))))

(defun +tasks--replace-body (bounds text)
  "Swap in TEXT and drop the element cache, which misparses large table rewrites."
  (unless (equal text (buffer-substring-no-properties (car bounds) (cdr bounds)))
    (goto-char (car bounds))
    (delete-region (car bounds) (cdr bounds))
    (insert text)
    (org-element-cache-reset)))

(defun +tasks/refresh-branches ()
  "Update the branches block of the current tasks.org."
  (interactive)
  (save-excursion
    (when-let ((bounds (+tasks--block-body-bounds)))
      (+tasks--replace-body bounds (concat (+tasks--current-table) "\n")))))

;;; capture and init

(defun +tasks--project-root ()
  (or (projectile-project-root) default-directory))

(defun +tasks--existing-project-file ()
  (let ((file (+tasks--project-file (+tasks--project-root))))
    (unless (file-exists-p file)
      (user-error "No %s in this project; run M-x +tasks/init" +tasks-file-name))
    file))

(defun +tasks/init ()
  "Write the tasks.org skeleton at the project root and open it."
  (interactive)
  (let ((file (+tasks--project-file (+tasks--project-root))))
    (when (file-exists-p file)
      (user-error "%s already exists" file))
    (with-temp-file file (insert (+tasks--skeleton)))
    (+tasks--exclude-from-git)
    (find-file file)))

(defun +tasks--excluded-files ()
  (list +tasks-file-name (concat +tasks-file-name "_archive")))

(defun +tasks--exclude-lines (existing)
  (seq-difference (+tasks--excluded-files) (split-string existing "\n" t)))

(defun +tasks--exclude-file ()
  (when-let* ((path (car (+tasks--git-lines "rev-parse" "--git-path" "info/exclude"))))
    (expand-file-name path)))

(defun +tasks--exclude-from-git ()
  "Keep tasks.org and its archive out of git via .git/info/exclude."
  (when-let* ((file (+tasks--exclude-file)))
    (let ((missing (+tasks--exclude-lines (if (file-exists-p file) (+tasks--read-file file) ""))))
      (when missing
        (make-directory (file-name-directory file) t)
        (write-region (concat (mapconcat #'identity missing "\n") "\n") nil file t)))))

(defun +tasks--read-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defconst +tasks--capture-template
  '("pi" "Project idea" entry
    (file+headline +tasks--existing-project-file "Ideas")
    "* IDEA %?\n%i" :prepend t))

(after! org-capture
  (add-to-list 'org-capture-templates +tasks--capture-template t))

;;; starting work on a task

(defconst +tasks-slug-max 40)

(defconst +tasks-start-states '("IDEA" "TODO"))

(defconst +tasks-in-flight-states '("TODO" "PLAN" "RED" "GREEN" "REVIEW"))

(defconst +tasks--new-idea "[new idea named after the branch]")

(defun +tasks--tasks-file-p (file)
  (and file (equal (file-name-nondirectory file) +tasks-file-name) t))

(defun +tasks--ascii (text)
  (require 'ucs-normalize)
  (concat (seq-filter (lambda (char) (< char 128)) (ucs-normalize-NFKD-string text))))

(defun +tasks--slug-words (heading)
  (split-string (downcase (+tasks--ascii heading)) "[^a-z0-9]+" t))

(defun +tasks--join-within (words limit)
  (cl-loop with slug = (car words)
           for word in (cdr words)
           for longer = (concat slug "-" word)
           while (<= (length longer) limit)
           do (setq slug longer)
           finally return slug))

(defun +tasks--slug (heading)
  (let ((slug (or (+tasks--join-within (+tasks--slug-words heading) +tasks-slug-max) "task")))
    (substring slug 0 (min (length slug) +tasks-slug-max))))

(defun +tasks--branch-name (prefix heading)
  (concat prefix (+tasks--slug heading)))

(defun +tasks--strip-prefix (branch prefixes)
  (if-let* ((prefix (seq-find (lambda (prefix) (string-prefix-p prefix branch)) prefixes)))
      (substring branch (length prefix))
    branch))

(defun +tasks--branch-title (branch prefixes)
  (string-replace "-" " " (+tasks--strip-prefix branch prefixes)))

(defun +tasks--feature-prompt (heading &optional accept)
  (concat (format "/feature %s" heading)
          (if accept (format "\n\nACCEPT: %s" accept) "")))

(defun +tasks--task-entry (states)
  (when (member (org-get-todo-state) states)
    (cons (org-get-heading t t t t) (point))))

(defun +tasks--open-tasks (&optional states)
  "Tasks in the current buffer whose state is in STATES, as (HEADING . POS)."
  (let ((states (or states +tasks-start-states)))
    (save-restriction
      (widen)
      (delq nil (org-map-entries (lambda () (+tasks--task-entry states)))))))

(defun +tasks--top-heading-end (title)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (format "^\\* %s[ \t]*$" (regexp-quote title)))
    (org-end-of-subtree t t)
    (point)))

(defun +tasks--parent-title ()
  (save-excursion
    (when (org-up-heading-safe)
      (org-get-heading t t t t))))

(defun +tasks--move-under (title)
  (unless (equal title (+tasks--parent-title))
    (let ((tree (buffer-substring (point) (save-excursion (org-end-of-subtree t t) (point)))))
      (delete-region (point) (save-excursion (org-end-of-subtree t t) (point)))
      (goto-char (+tasks--top-heading-end title))
      (org-paste-subtree 2 tree))))

(defun +tasks--put-missing (property value)
  (when (and value (not (org-entry-get nil property)))
    (org-entry-put nil property value)))

(defun +tasks--start-entry (branch accept)
  "Link the task at point to BRANCH, make it a TODO and move it under * Active."
  (org-back-to-heading t)
  (org-entry-put nil "BRANCH" branch)
  (+tasks--put-missing "ACCEPT" accept)
  (+tasks--put-missing "MODEL" "local")
  (when (equal (org-get-todo-state) "IDEA")
    (org-todo "TODO"))
  (+tasks--move-under "Active"))

(defun +tasks--insert-idea (title branch)
  (goto-char (+tasks--top-heading-end "Ideas"))
  (insert "** IDEA " title "\n")
  (forward-line -1)
  (org-entry-put nil "BRANCH" branch))

(defun +tasks--tasks-buffer ()
  (find-file-noselect (+tasks--existing-project-file)))

(defun +tasks--task-at-point (states)
  (when (and (+tasks--tasks-file-p buffer-file-name)
             (derived-mode-p 'org-mode)
             (not (org-before-first-heading-p)))
    (save-excursion
      (org-back-to-heading t)
      (when (member (org-get-todo-state) states)
        (cons (current-buffer) (point))))))

(defun +tasks--pick-task (states)
  (let* ((buffer (+tasks--tasks-buffer))
         (tasks (with-current-buffer buffer (+tasks--open-tasks states))))
    (unless tasks
      (user-error "No %s task in %s" (string-join states "/") (buffer-file-name buffer)))
    (cons buffer (cdr (assoc (completing-read "Task: " tasks nil t) tasks)))))

(defun +tasks--chosen-task (&optional states)
  (let ((states (or states +tasks-start-states)))
    (or (+tasks--task-at-point states) (+tasks--pick-task states))))

(defun +tasks--read-prefix ()
  (if (cdr +tasks-branch-prefixes)
      (completing-read "Branch prefix: " +tasks-branch-prefixes nil t)
    (car +tasks-branch-prefixes)))

(defun +tasks--read-accept (heading)
  (let ((accept (string-trim (read-string (format "ACCEPT for \"%s\" (one testable line): " heading)))))
    (unless (string-empty-p accept) accept)))

(defun +tasks--checkout-branch (branch)
  (unless (if (+tasks--git-ok-p "show-ref" "--verify" "--quiet" (concat "refs/heads/" branch))
              (+tasks--git-ok-p "switch" branch)
            (+tasks--git-ok-p "switch" "-c" branch))
    (user-error "git could not switch to %s; check uncommitted changes" branch))
  (when (fboundp 'magit-refresh-all)
    (magit-refresh-all)))

(cl-defun +tasks--hand-off (root &key draft prepare session model)
  "Give a task to pi in ROOT; PREPARE runs only once the repo review allows it."
  (if (fboundp '+ai--launch-pi)
      (+ai--launch-pi root :draft draft :prepare prepare :session session :model model)
    (when prepare
      (funcall prepare))
    (when draft
      (kill-new draft)
      (message "Copied: %s" draft))))

(defun +tasks--proposed-branch (heading)
  (or (org-entry-get nil "BRANCH")
      (+tasks--branch-name (+tasks--read-prefix) heading)))

(defun +tasks--start-at (marker branch accept)
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (+tasks--checkout-branch branch)
      (+tasks--start-entry branch accept)
      (+tasks/refresh-branches)
      (save-buffer))))

(defun +tasks/start ()
  "Branch, link, promote and refile a task, then open pi on it.
Uses the IDEA or TODO at point in tasks.org, otherwise asks for one.
Nothing changes until the repo review lets pi start."
  (interactive)
  (pcase-let ((`(,buffer . ,pos) (+tasks--chosen-task)))
    (with-current-buffer buffer
      (goto-char pos)
      (let* ((marker (point-marker))
             (heading (org-get-heading t t t t))
             (branch (read-string "Branch: " (+tasks--proposed-branch heading)))
             (accept (unless (org-entry-get nil "ACCEPT") (+tasks--read-accept heading))))
        (+tasks--hand-off (+tasks--project-root)
                          :draft (+tasks--feature-prompt heading (or accept (org-entry-get nil "ACCEPT")))
                          :prepare (lambda () (+tasks--start-at marker branch accept))
                          :session branch
                          :model (or (org-entry-get nil "MODEL") "local"))))))

(defun +tasks/accept ()
  "Set the one-line acceptance criterion of a task."
  (interactive)
  (pcase-let ((`(,buffer . ,pos) (+tasks--chosen-task
                                  (seq-uniq (append +tasks-start-states +tasks-in-flight-states)))))
    (with-current-buffer buffer
      (goto-char pos)
      (org-entry-put nil "ACCEPT" (read-string "ACCEPT: " (org-entry-get nil "ACCEPT")))
      (save-buffer))))

(defun +tasks/link ()
  "Link the branch on the branches row at point to a task, or to a new idea."
  (interactive)
  (let* ((branch (or (+tasks--row-branch (thing-at-point 'line t))
                     (user-error "Point is not on a row of the branches block")))
         (tasks (+tasks--open-tasks org-not-done-keywords))
         (choice (completing-read (format "Link %s to: " branch)
                                  (cons +tasks--new-idea (mapcar #'car tasks)) nil t)))
    (save-excursion
      (if (equal choice +tasks--new-idea)
          (+tasks--insert-idea (+tasks--branch-title branch +tasks-branch-prefixes) branch)
        (goto-char (cdr (assoc choice tasks)))
        (org-entry-put nil "BRANCH" branch)))
    (+tasks/refresh-branches)))

(defun +tasks/idea ()
  "Capture an idea into this project's tasks.org."
  (interactive)
  (org-capture nil "pi"))

(defun +tasks/board ()
  "Open the Board agenda view."
  (interactive)
  (org-agenda nil "b"))

(defun +tasks/open ()
  "Visit this project's tasks.org."
  (interactive)
  (find-file (+tasks--existing-project-file)))

(defun +tasks/menu ()
  "Open the tasks menu."
  (interactive)
  (unless (fboundp '+tasks-transient)
    (load (expand-file-name "menu" +tasks--dir) nil t))
  (call-interactively #'+tasks-transient))

;;; review, test, done

(defun +tasks--task-plist ()
  (list :buffer (current-buffer)
        :heading (org-get-heading t t t t)
        :branch (org-entry-get nil "BRANCH")
        :accept (org-entry-get nil "ACCEPT")
        :model (org-entry-get nil "MODEL")))

(defun +tasks--linked-task-at-point ()
  (when (and (+tasks--tasks-file-p buffer-file-name)
             (derived-mode-p 'org-mode)
             (not (org-before-first-heading-p)))
    (save-excursion
      (org-back-to-heading t)
      (when (org-entry-get nil "BRANCH")
        (+tasks--task-plist)))))

(defun +tasks--task-on-current-branch ()
  (when-let* ((branch (car (+tasks--git-lines "branch" "--show-current"))))
    (with-current-buffer (+tasks--tasks-buffer)
      (when-let* ((task (+tasks--branch-task branch)))
        (save-excursion
          (goto-char (nth 2 task))
          (+tasks--task-plist))))))

(defun +tasks--linked-p (entry)
  (save-excursion
    (goto-char (cdr entry))
    (org-entry-get nil "BRANCH")))

(defun +tasks--pick-linked-task ()
  (with-current-buffer (+tasks--tasks-buffer)
    (let ((tasks (seq-filter #'+tasks--linked-p (+tasks--open-tasks org-not-done-keywords))))
      (unless tasks
        (user-error "No open task with a :BRANCH: in %s" buffer-file-name))
      (save-excursion
        (goto-char (cdr (assoc (completing-read "Task: " tasks nil t) tasks)))
        (+tasks--task-plist)))))

(defun +tasks--work-task ()
  "The task to review, test or finish: at point, on the current branch, or picked."
  (or (+tasks--linked-task-at-point)
      (+tasks--task-on-current-branch)
      (+tasks--pick-linked-task)))

(defun +tasks--task-root (task)
  (buffer-local-value 'default-directory (plist-get task :buffer)))

(defun +tasks--range (main branch)
  (format "%s...%s" main branch))

(defun +tasks/resume ()
  "Switch to a started task's branch and focus its pi session."
  (interactive)
  (let* ((task (+tasks--work-task))
         (branch (plist-get task :branch))
         (root (+tasks--task-root task)))
    (+tasks--hand-off root
                      :prepare (lambda () (let ((default-directory root)) (+tasks--switch branch)))
                      :session branch
                      :model (plist-get task :model))))

(defun +tasks/review ()
  "Show the task branch's whole diff against the default branch, with its ACCEPT."
  (interactive)
  (let* ((task (+tasks--work-task))
         (default-directory (+tasks--task-root task)))
    (magit-diff-range (+tasks--range (+tasks--default-branch) (plist-get task :branch)))
    (message "ACCEPT: %s" (or (plist-get task :accept) "(none set)"))))

(defun +tasks--review-prompt (accept diff)
  (format "Acceptance criterion: %s

Review this diff against it: what does not serve the criterion, missing or weak tests, risky changes.

%s" (or accept "(none set)") diff))

(defun +tasks--review-buffer (branch)
  (with-current-buffer (get-buffer-create (format "*tasks-review:%s*" branch))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "Reviewing %s on %s...\n\n" branch (or (bound-and-true-p +ai-fast-model) "gptel"))))
    (current-buffer)))

(defun +tasks--review-callback (buffer)
  (lambda (response info)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (if (stringp response)
                    response
                  (format "Review failed: %s" (plist-get info :status))))))))

(defun +tasks/ai-review ()
  "Ask the local model, fresh, to review the task branch against its ACCEPT."
  (interactive)
  (require 'gptel)
  (let* ((task (+tasks--work-task))
         (default-directory (+tasks--task-root task))
         (branch (plist-get task :branch))
         (diff (string-join (+tasks--git-lines "diff" (+tasks--range (+tasks--default-branch) branch)) "\n"))
         (buffer (+tasks--review-buffer branch))
         (gptel-model (or (bound-and-true-p +ai-fast-model) gptel-model)))
    (pop-to-buffer buffer)
    (gptel-request (+tasks--review-prompt (plist-get task :accept) diff)
      :system (alist-get 'review gptel-directives)
      :callback (+tasks--review-callback buffer))))

(defun +tasks--test-command (justfile-text)
  (when (and justfile-text
             (string-match-p "^@?test\\(?:[ \t][^:\n]*\\)?:\\(?:[^=]\\|$\\)" justfile-text))
    "just test"))

(defun +tasks--justfile-text (root)
  (when-let* ((file (seq-find #'file-exists-p
                              (mapcar (lambda (name) (expand-file-name name root))
                                      '("justfile" "Justfile" ".justfile")))))
    (+tasks--read-file file)))

(defun +tasks--choose-test-command (recipe-p just-p projectile-command)
  (if (and recipe-p just-p) "just test" projectile-command))

(defun +tasks--projectile-test-command (root)
  (when (fboundp 'projectile-test-command)
    (let ((command (ignore-errors (projectile-test-command root))))
      (and (stringp command) command))))

(defun +tasks--warn-missing-just (recipe-p just-p)
  (when (and recipe-p (not just-p))
    (message "justfile has a test recipe but `just' is not on exec-path; using projectile's test command")))

(defun +tasks--project-test-command (root)
  (let ((recipe-p (+tasks--test-command (+tasks--justfile-text root)))
        (just-p (executable-find "just")))
    (+tasks--warn-missing-just recipe-p just-p)
    (+tasks--choose-test-command recipe-p just-p (+tasks--projectile-test-command root))))

(defun +tasks/test ()
  "Run the project's tests: `just test' when available, else projectile's command."
  (interactive)
  (let* ((default-directory (+tasks--project-root))
         (command (+tasks--project-test-command default-directory)))
    (if command
        (compile command)
      (call-interactively #'projectile-test-project))))

(defun +tasks--merge-message (branch heading)
  (format "Merge %s: %s" branch heading))

(defun +tasks--dirty-p ()
  (+tasks--git-lines "status" "--porcelain" "--untracked-files=no"))

(defun +tasks--switch (branch)
  (unless (+tasks--git-ok-p "switch" branch)
    (user-error "git could not switch to %s" branch)))

(defun +tasks--run-tests-or-confirm (root)
  (if-let* ((command (+tasks--project-test-command root)))
      (let ((buffer (get-buffer-create "*tasks-done-tests*")))
        (with-current-buffer buffer (erase-buffer))
        (message "Running %s..." command)
        (unless (zerop (call-process-shell-command command nil buffer))
          (display-buffer buffer)
          (user-error "%s failed; nothing merged" command)))
    (unless (y-or-n-p "No test command found; merge without running tests? ")
      (user-error "Stopped before merging"))))

(defun +tasks--catch-up (branch main)
  (when (and (not (+tasks--git-ok-p "merge-base" "--is-ancestor" branch main))
             (+tasks--git-ok-p "merge-base" "--is-ancestor" branch (concat "origin/" main)))
    (unless (+tasks--git-ok-p "merge" "--ff-only" (concat "origin/" main))
      (user-error "%s is merged on origin, but %s cannot fast-forward to it" branch main))))

(defun +tasks--merge (branch main heading)
  (+tasks--switch main)
  (+tasks--catch-up branch main)
  (unless (+tasks--git-ok-p "merge-base" "--is-ancestor" branch main)
    (unless (+tasks--git-ok-p "merge" "--no-ff" "-m" (+tasks--merge-message branch heading) branch)
      (user-error "Merging %s stopped; finish it in magit, then run done again" branch))))

(defun +tasks--delete-local-branch (branch)
  "Delete BRANCH once HEAD contains it; plain -d also wants its upstream to."
  (unless (and (+tasks--git-ok-p "merge-base" "--is-ancestor" branch "HEAD")
               (+tasks--git-ok-p "branch" "-D" branch))
    (user-error "%s is not merged into HEAD; kept it" branch)))

(defun +tasks--delete-branch (branch)
  (+tasks--delete-local-branch branch)
  (when (and (+tasks--git-ok-p "show-ref" "--verify" "--quiet" (concat "refs/remotes/origin/" branch))
             (y-or-n-p (format "Also delete origin/%s? " branch)))
    (+tasks--git-ok-p "push" "origin" "--delete" branch)))

(defun +tasks--mark-done (branch buffer)
  (with-current-buffer buffer
    (goto-char (nth 2 (+tasks--branch-task branch)))
    (org-todo "DONE")
    (+tasks/refresh-branches)
    (save-buffer)))

(defun +tasks/done ()
  "Test the task's branch, merge it --no-ff, delete it and mark the task DONE."
  (interactive)
  (let* ((task (+tasks--work-task))
         (branch (plist-get task :branch))
         (default-directory (+tasks--task-root task))
         (main (+tasks--default-branch)))
    (when (+tasks--dirty-p)
      (user-error "Commit or stash your changes first"))
    (+tasks--switch branch)
    (+tasks--run-tests-or-confirm default-directory)
    (+tasks--merge branch main (plist-get task :heading))
    (+tasks--delete-branch branch)
    (+tasks--mark-done branch (plist-get task :buffer))
    (when (fboundp 'magit-refresh-all)
      (magit-refresh-all))
    (message "%s merged into %s; task done" branch main)))

;;; tasks.org buffers

(defun +tasks--refresh-on-save ()
  (condition-case err
      (+tasks/refresh-branches)
    (error (message "branches block not refreshed: %s" (error-message-string err)))))

(define-minor-mode +tasks-file-mode
  "Branch refresh on save and the tasks menu for a project's tasks.org."
  :keymap (make-sparse-keymap)
  (if +tasks-file-mode
      (add-hook 'before-save-hook #'+tasks--refresh-on-save nil t)
    (remove-hook 'before-save-hook #'+tasks--refresh-on-save t)))

(defun +tasks--project-category (file)
  (file-name-nondirectory (directory-file-name (file-name-directory file))))

(defun +tasks--enable-file-mode-h ()
  (when (+tasks--tasks-file-p buffer-file-name)
    (setq-local org-category (+tasks--project-category buffer-file-name))
    (+tasks-file-mode 1)))

(add-hook! 'org-mode-hook #'+tasks--enable-file-mode-h)

(map! :leader :desc "tasks" "5" #'+tasks/menu)

(map! :map +tasks-file-mode-map
      :localleader
      :desc "tasks menu" "w" #'+tasks/menu)
