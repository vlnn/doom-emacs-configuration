;;; my/tasks/config.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar gptel-model)

(defconst +tasks--dir
  (file-name-directory (or load-file-name (bound-and-true-p byte-compile-current-file) buffer-file-name)))

(defvar +tasks-file-name "tasks.org")

(defvar +tasks-worktree-dir ".worktrees"
  "Where task worktrees live, relative to the repo's main checkout.")

(defvar +tasks-branch-prefixes '("feat/")
  "Git branch prefixes the branches block treats as tracked work.")

(defvar +tasks-agenda-commands '(org-agenda org-agenda-list org-todo-list)
  "Agenda entry points that refresh `org-agenda-files' first.")

(defconst +tasks--skeleton-text
  "#+TODO: IDEA(i) TODO(t) PLAN(p!) RED(r!) GREEN(g!) REVIEW(v!) WAIT(w!) | DONE(d!) DROPPED(x@)
#+COLUMNS: %45ITEM %8TODO %18BRANCH %7MODEL %12CLOSED
#+PROPERTY: MODEL_ALL local claude
#+PROPERTY: PLAN_MODEL_ALL local claude
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

(load! "plan")

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

(defvar +tasks--added-agenda-files nil
  "The tasks.org files the last refresh added to `org-agenda-files'.")

(defun +tasks--agenda-base (current added)
  "CURRENT `org-agenda-files' as a list, less the tasks.org files a refresh ADDED to it.
Empty, it is `org-directory', as Doom's default would have it."
  (or (seq-difference (if (listp current) current (org-agenda-files t)) added)
      (list org-directory)))

(defun +tasks--refresh-agenda-files (&rest _)
  "Add each known project's tasks.org to `org-agenda-files', keeping whatever else it holds."
  (let ((base (+tasks--agenda-base org-agenda-files +tasks--added-agenda-files)))
    (setq org-agenda-files (+tasks--agenda-files base (bound-and-true-p projectile-known-projects))
          +tasks--added-agenda-files (seq-difference org-agenda-files base))
    org-agenda-files))

(defun +tasks--advise-agenda-commands ()
  (dolist (command +tasks-agenda-commands)
    (advice-add command :before #'+tasks--refresh-agenda-files)))

(defconst +tasks--board-prefix-width 24)

(defconst +tasks--board-prefix-format
  `((todo . ,(format " %%-12:c %%-%d(+tasks--agenda-prefix) " +tasks--board-prefix-width))))

(defun +tasks--agenda-prefix ()
  "What the board shows before a task: why it waits, what its plan needs, else its branch, else nothing."
  (truncate-string-to-width (or (and (equal (org-get-todo-state) "WAIT") (org-entry-get nil "REASON"))
                                (+tasks--plan-mark-at-point)
                                (org-entry-get nil "BRANCH")
                                "")
                            +tasks--board-prefix-width nil nil "…"))

(defun +tasks--board-block (state header &optional leaves-only)
  `(todo ,state ((org-agenda-overriding-header ,header)
                 (org-agenda-prefix-format ',+tasks--board-prefix-format)
                 ,@(when leaves-only '((org-agenda-skip-function #'+tasks--skip-unless-leaf))))))

(defconst +tasks--board-stages
  '(("IDEA" "Ideas" t)
    ("TODO" "Ready" t)
    ("PLAN" "Plan: interview running, or waiting for your approval")
    ("RED" "Red: pi is working")
    ("GREEN" "Green: tests pass, review it")
    ("REVIEW" "Review: needs you")
    ("WAIT" "Waiting: on something outside the repo")
    ("DONE|DROPPED" "Done: archive when ready"))
  "The board's blocks in pipeline order: STATE HEADER [LEAVES-ONLY].")

(defconst +tasks--agenda-custom-commands
  `(("b" "Board" ,(mapcar (lambda (stage) (apply #'+tasks--board-block stage)) +tasks--board-stages))
    ("w" "Last week" agenda ""
     ((org-agenda-span 7)
      (org-agenda-start-day "-7d")
      (org-agenda-start-with-log-mode '(state closed))))
    ("r" "Needs my review" todo "REVIEW")))

(defun +tasks--with-agenda-commands (commands ours)
  "COMMANDS plus OURS, which replace any command under the same key."
  (append (seq-remove (lambda (command) (assoc (car command) ours)) commands) ours))

(after! org-agenda
  (+tasks--advise-agenda-commands)
  (setq org-agenda-custom-commands
        (+tasks--with-agenda-commands org-agenda-custom-commands +tasks--agenda-custom-commands)))

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

(defun +tasks--main-root (dir)
  "The main checkout of the repo DIR is in, so a task worktree finds its tasks.org."
  (let* ((default-directory dir)
         (common (car (+tasks--git-lines "rev-parse" "--path-format=absolute" "--git-common-dir"))))
    (if (and common (equal (file-name-nondirectory common) ".git"))
        (file-name-directory common)
      dir)))

(defun +tasks--existing-project-file ()
  (let ((file (+tasks--project-file (+tasks--main-root (+tasks--project-root)))))
    (unless (file-exists-p file)
      (user-error "No %s in this project; run M-x +tasks/init" +tasks-file-name))
    file))

(defun +tasks/init ()
  "Write the tasks.org skeleton at the project root and open it.
Offers the grilling skill and an AGENTS.md too, asking before each."
  (interactive)
  (let* ((root (+tasks--main-root (+tasks--project-root)))
         (file (+tasks--project-file root)))
    (when (file-exists-p file)
      (user-error "%s already exists" file))
    (with-temp-file file (insert (+tasks--skeleton)))
    (+tasks--exclude-from-git)
    (+tasks--offer-agent-inputs root)
    (find-file file)))

(defun +tasks--excluded-files ()
  (list +tasks-file-name (concat +tasks-file-name "_archive") (concat +tasks-worktree-dir "/")
        (concat "/" +tasks-plan-file-name)))

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

(defconst +tasks-in-flight-states '("PLAN" "RED" "GREEN" "REVIEW" "WAIT"))

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

(defun +tasks--has-open-descendant-p ()
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t))))
      (cl-loop while (and (outline-next-heading) (< (point) end))
               thereis (member (org-get-todo-state) org-not-done-keywords)))))

(defun +tasks--skip-unless-leaf ()
  (when (+tasks--has-open-descendant-p)
    (save-excursion (outline-next-heading) (point))))

(defun +tasks--task-ancestor-p ()
  (save-excursion (and (org-up-heading-safe) (org-get-todo-state))))

(defun +tasks--goto-tree-root ()
  (org-back-to-heading t)
  (while (+tasks--task-ancestor-p)
    (org-up-heading-safe)))

(defun +tasks--ancestor-titles ()
  (save-excursion
    (org-back-to-heading t)
    (let (titles)
      (while (+tasks--task-ancestor-p)
        (org-up-heading-safe)
        (push (org-get-heading t t t t) titles))
      titles)))

(defun +tasks--task-parents ()
  (when-let* ((titles (+tasks--ancestor-titles)))
    (string-join titles " / ")))

(defun +tasks--task-path ()
  (string-join (append (+tasks--ancestor-titles)
                       (list (save-excursion (org-back-to-heading t) (org-get-heading t t t t))))
               " / "))

(defun +tasks--startable-entry ()
  (when (and (member (org-get-todo-state) +tasks-start-states)
             (not (+tasks--has-open-descendant-p)))
    (list (+tasks--task-path) (point) (org-current-level)
          (save-excursion (+tasks--goto-tree-root) (point)))))

(defun +tasks--tree-then-deepest-p (a b)
  (pcase-let ((`(,_ ,_ ,level-a ,root-a) a)
              (`(,_ ,_ ,level-b ,root-b) b))
    (or (< root-a root-b)
        (and (= root-a root-b) (> level-a level-b)))))

(defun +tasks--subtree-bounds ()
  (save-excursion
    (org-back-to-heading t)
    (cons (point) (org-end-of-subtree t t))))

(defun +tasks--within-p (bounds entry)
  (or (null bounds)
      (<= (car bounds) (nth 1 entry) (1- (cdr bounds)))))

(defun +tasks--startable-tasks (&optional within-tree)
  "Leaf IDEA/TODO tasks as (PATH . POS): trees in file order, deepest leaves first.
With WITHIN-TREE, only leaves in the subtree at point."
  (let ((bounds (and within-tree (+tasks--subtree-bounds))))
    (save-restriction
      (widen)
      (mapcar (pcase-lambda (`(,path ,pos . ,_)) (cons path pos))
              (sort (seq-filter (apply-partially #'+tasks--within-p bounds)
                                (delq nil (org-map-entries #'+tasks--startable-entry)))
                    #'+tasks--tree-then-deepest-p)))))

(defun +tasks--insert-child-idea (title)
  "Add an IDEA TITLE as the last child of the heading at point."
  (org-back-to-heading t)
  (let ((stars (make-string (1+ (org-current-level)) ?*)))
    (org-end-of-subtree t t)
    (unless (bolp) (insert "\n"))
    (insert stars " IDEA " title "\n")))

(defun +tasks--task-entry (states)
  (when (member (org-get-todo-state) states)
    (cons (org-get-heading t t t t) (point))))

(defun +tasks--open-tasks (&optional states)
  "Tasks in the current buffer whose state is in STATES, as (HEADING . POS)."
  (let ((states (or states +tasks-start-states)))
    (save-restriction
      (widen)
      (delq nil (org-map-entries (lambda () (+tasks--task-entry states)))))))

(defconst +tasks--cookie-regexp "[ \t]*\\[[0-9]*\\(?:%\\|/[0-9]*\\)\\]")

(defun +tasks--heading-title ()
  "The heading at point as you named it: no TODO keyword, priority, tags or statistics cookies."
  (string-trim (replace-regexp-in-string +tasks--cookie-regexp "" (org-get-heading t t t t))))

(defun +tasks--top-heading (title)
  "Start of the top-level heading named TITLE, or a user-error naming it."
  (save-excursion
    (goto-char (point-min))
    (or (cl-loop while (re-search-forward "^\\* " nil t)
                 thereis (and (equal title (+tasks--heading-title)) (line-beginning-position)))
        (user-error "No top-level \"* %s\" heading in %s" title (buffer-name)))))

(defun +tasks--top-heading-end (title)
  (save-excursion
    (goto-char (+tasks--top-heading title))
    (org-end-of-subtree t t)
    (point)))

(defun +tasks--parent-title ()
  (save-excursion
    (when (org-up-heading-safe)
      (+tasks--heading-title))))

(defun +tasks--move-under (title)
  "Move the subtree at point under the top-level heading TITLE, which must exist first."
  (unless (equal title (+tasks--parent-title))
    (+tasks--top-heading title)
    (let ((tree (buffer-substring (point) (save-excursion (org-end-of-subtree t t) (point)))))
      (delete-region (point) (save-excursion (org-end-of-subtree t t) (point)))
      (goto-char (+tasks--top-heading-end title))
      (org-paste-subtree 2 tree))))

(defun +tasks--put-missing (property value)
  (when (and value (not (org-entry-get nil property)))
    (org-entry-put nil property value)))

(defun +tasks--start-entry (branch accept &optional plan-model)
  "Link the task at point to BRANCH, make it a PLAN and move its tree under * Active."
  (org-back-to-heading t)
  (org-entry-put nil "BRANCH" branch)
  (+tasks--put-missing "ACCEPT" accept)
  (+tasks--put-missing "MODEL" "local")
  (when plan-model (org-entry-put nil "PLAN_MODEL" plan-model))
  (org-todo "PLAN")
  (+tasks--goto-tree-root)
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

(defun +tasks--in-order (candidates)
  "Completion table over CANDIDATES that completion UIs must not re-sort."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity) (cycle-sort-function . identity))
      (complete-with-action action candidates string pred))))

(defun +tasks--read-task (buffer tasks what)
  (unless tasks
    (user-error "No %s task in %s" what (buffer-file-name buffer)))
  (cons buffer (cdr (assoc (completing-read "Task: " (+tasks--in-order tasks) nil t) tasks))))

(defun +tasks--pick-task (states)
  (let ((buffer (+tasks--tasks-buffer)))
    (+tasks--read-task buffer (with-current-buffer buffer (+tasks--open-tasks states))
                       (string-join states "/"))))

(defun +tasks--task-to-start ()
  "The leaf at point, a leaf under the parent at point, or a picked leaf."
  (pcase (+tasks--task-at-point +tasks-start-states)
    (`(,buffer . ,pos)
     (save-excursion
       (goto-char pos)
       (if (+tasks--has-open-descendant-p)
           (+tasks--read-task buffer (+tasks--startable-tasks t) "leaf IDEA/TODO")
         (cons buffer pos))))
    (_ (let ((buffer (+tasks--tasks-buffer)))
         (+tasks--read-task buffer (with-current-buffer buffer (+tasks--startable-tasks))
                            "leaf IDEA/TODO")))))

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

(defun +tasks--worktrees-dir-regexp ()
  (concat "[/\\\\]" (regexp-quote +tasks-worktree-dir) "\\'"))

(after! lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories (+tasks--worktrees-dir-regexp)))

(defun +tasks--worktree-path (root branch)
  (file-name-as-directory
   (expand-file-name (string-replace "/" "-" branch) (expand-file-name +tasks-worktree-dir root))))

(defun +tasks--parse-worktrees (lines)
  "(BRANCH . DIR) for each worktree in `git worktree list --porcelain' LINES."
  (let (dir result)
    (dolist (line lines (nreverse result))
      (cond ((string-prefix-p "worktree " line)
             (setq dir (file-name-as-directory (substring line (length "worktree ")))))
            ((string-prefix-p "branch refs/heads/" line)
             (push (cons (substring line (length "branch refs/heads/")) dir) result))))))

(defun +tasks--branch-worktree-in (root branch)
  (let ((default-directory root))
    (cdr (assoc branch (+tasks--parse-worktrees (+tasks--git-lines "worktree" "list" "--porcelain"))))))

(defun +tasks--task-worktree (root branch)
  "Where BRANCH's work happens: the worktree it is checked out in, else the one start will add."
  (or (+tasks--branch-worktree-in root branch) (+tasks--worktree-path root branch)))

(defun +tasks--worktree-add-args (tree branch)
  (if (+tasks--git-ok-p "show-ref" "--verify" "--quiet" (concat "refs/heads/" branch))
      (list tree branch)
    (list "-b" branch tree)))

(defun +tasks--add-worktree (root branch)
  "The worktree BRANCH is checked out in, adding one under ROOT's `+tasks-worktree-dir' if none."
  (let ((default-directory root))
    (or (+tasks--branch-worktree-in root branch)
        (let ((tree (+tasks--worktree-path root branch)))
          (+tasks--exclude-from-git)
          (unless (apply #'+tasks--git-ok-p "worktree" "add" "--quiet" (+tasks--worktree-add-args tree branch))
            (user-error "git could not add a worktree for %s at %s" branch tree))
          (when (fboundp 'magit-refresh-all)
            (magit-refresh-all))
          tree))))

(cl-defun +tasks--hand-off (root &key draft send prepare session model review-root)
  "Give a task to pi in ROOT; PREPARE runs only once REVIEW-ROOT's review allows it."
  (if (fboundp '+ai--launch-pi)
      (+ai--launch-pi root :draft draft :send send :prepare prepare :session session :model model
                      :review-root review-root)
    (when prepare
      (funcall prepare))
    (when draft
      (kill-new draft)
      (message "Copied: %s" draft))))

(defun +tasks--proposed-branch (heading)
  (or (org-entry-get nil "BRANCH")
      (+tasks--branch-name (+tasks--read-prefix) heading)))

(defun +tasks--start-at (marker root branch accept &optional plan-model)
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (+tasks--top-heading "Active")
      (goto-char marker)
      (+tasks--ensure-skill root (+tasks--add-worktree root branch))
      (+tasks--start-entry branch accept plan-model)
      (+tasks/refresh-branches)
      (save-buffer))))

(defun +tasks/start (&optional pick-plan-model)
  "Branch into a worktree, link, promote and refile a task, then open pi's interview there.
Only leaves start: on a parent with open sub-tasks, asks for one of its
leaves; elsewhere, asks for any leaf, deepest first within each tree.
With PICK-PLAN-MODEL (\\[universal-argument]) asks which model runs the interview.
Nothing changes until the repo review lets pi start."
  (interactive "P")
  (pcase-let ((`(,buffer . ,pos) (+tasks--task-to-start)))
    (with-current-buffer buffer
      (+tasks--top-heading "Active")
      (goto-char pos)
      (let* ((marker (point-marker))
             (root (file-name-as-directory (file-truename default-directory)))
             (heading (org-get-heading t t t t))
             (branch (read-string "Branch: " (+tasks--proposed-branch heading)))
             (accept (unless (org-entry-get nil "ACCEPT") (+tasks--read-accept heading)))
             (plan-model (and pick-plan-model (+tasks--read-plan-model)))
             (task (+tasks--task-plist)))
        (+tasks--check-models (+tasks--work-model task) (or plan-model (+tasks--interview-model task)))
        (+tasks--hand-off (+tasks--task-worktree root branch)
                          :review-root root
                          :draft (+tasks--interview-prompt heading (or accept (org-entry-get nil "ACCEPT"))
                                                           (+tasks--task-parents))
                          :prepare (lambda () (+tasks--start-at marker root branch accept plan-model))
                          :session branch
                          :model (or plan-model (+tasks--interview-model task)))))))

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

(defun +tasks/sub-idea ()
  "Add an IDEA under the open task at point, or under a picked one."
  (interactive)
  (pcase-let ((`(,buffer . ,pos) (or (+tasks--task-at-point org-not-done-keywords)
                                     (with-current-buffer (+tasks--tasks-buffer)
                                       (+tasks--pick-task org-not-done-keywords)))))
    (with-current-buffer buffer
      (goto-char pos)
      (let ((parent (org-get-heading t t t t)))
        (+tasks--insert-child-idea (read-string (format "Sub-idea of \"%s\": " parent))))
      (save-buffer))))

(defun +tasks/idea ()
  "Capture an idea into this project's tasks.org."
  (interactive)
  (org-capture nil "pi"))

(defvar +tasks-board-scope 'all
  "Which projects the board shows: `all', or `project' for the one it is opened from.")

(defun +tasks--project-filter (root)
  (list (concat "+" (+tasks--project-category (+tasks--project-file root)))))

(defun +tasks--board-filter (scope root)
  (when (eq scope 'project)
    (+tasks--project-filter root)))

(defun +tasks/board ()
  "Open the Board agenda view, narrowed to this project when `+tasks-board-scope' says so."
  (interactive)
  (let ((filter (+tasks--board-filter +tasks-board-scope (+tasks--project-root))))
    (org-agenda nil "b")
    (org-agenda-filter-show-all-cat)
    (when filter
      (org-agenda-filter-apply filter 'category))))

(defun +tasks/toggle-board-scope ()
  "Flip the board between this project's tasks and every project's, and show it."
  (interactive)
  (setq +tasks-board-scope (if (eq +tasks-board-scope 'all) 'project 'all))
  (+tasks/board))

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

;;; stages

(defun +tasks--keep-reason (state reason)
  (if (equal state "WAIT")
      (when reason (org-entry-put nil "REASON" reason))
    (org-entry-delete nil "REASON")))

(defun +tasks--enter-state (state &optional reason)
  (org-back-to-heading t)
  (+tasks--keep-reason state reason)
  (org-todo state))

(cl-defun +tasks--set-state (task state &key reason)
  "Move TASK to STATE in its own buffer; REASON stays as :REASON: while it waits."
  (with-current-buffer (plist-get task :buffer)
    (save-excursion
      (goto-char (plist-get task :marker))
      (+tasks--enter-state state reason))
    (when buffer-file-name
      (save-buffer))))

(defun +tasks--ask-wait-reason ()
  "After a state change by hand: ask why a task waits, forget why once it moves on."
  (if (equal org-state "WAIT")
      (unless (org-entry-get nil "REASON")
        (org-entry-put nil "REASON" (read-string "Waiting on: ")))
    (org-entry-delete nil "REASON")))

(defun +tasks/wait ()
  "Park the task on something outside the repo, saying what."
  (interactive)
  (let ((task (+tasks--work-task)))
    (+tasks--set-state task "WAIT" :reason (read-string (format "%s waits on: " (plist-get task :heading))))))

(defun +tasks--test-verdict (compilation-message)
  (if (string-prefix-p "finished" compilation-message) "GREEN" "RED"))

(defvar-local +tasks--compiled-task nil
  "The task whose tests this compilation buffer runs.")

(defun +tasks--mark-compiled-tests (buffer compilation-message)
  (when-let* ((task (buffer-local-value '+tasks--compiled-task buffer))
              ((buffer-live-p (plist-get task :buffer))))
    (+tasks--set-state task (+tasks--test-verdict compilation-message))))

(add-hook 'compilation-finish-functions #'+tasks--mark-compiled-tests)

;;; next: the one key that moves the task at point along

(defconst +tasks--next-steps
  '(("IDEA" . +tasks/start) ("TODO" . +tasks/start)
    ("PLAN" . +tasks/approve-plan) ("RED" . +tasks/test)
    ("GREEN" . +tasks/review) ("REVIEW" . +tasks/done)
    ("WAIT" . +tasks/unpark)
    ("DONE" . org-archive-subtree) ("DROPPED" . org-archive-subtree))
  "For each state, the command that takes a task to its next stage.")

(defun +tasks--next-step (state)
  (cdr (assoc state +tasks--next-steps)))

(defun +tasks/unpark ()
  "Take a waiting task back to RED, or to PLAN while its plan is not approved, and reopen its pi session."
  (interactive)
  (let ((task (+tasks--work-task)))
    (+tasks--set-state task (+tasks--unparked-state task)))
  (+tasks/resume))

(defun +tasks--row-marker ()
  (and (derived-mode-p 'org-agenda-mode)
       (or (org-get-at-bol 'org-hd-marker) (org-get-at-bol 'org-marker))))

(defun +tasks--at-heading-of-row (thunk)
  "Run THUNK in the org buffer at the heading a board row points to, or here."
  (if-let* ((marker (+tasks--row-marker)))
      (progn
        (pop-to-buffer (marker-buffer marker))
        (goto-char marker)
        (funcall thunk))
    (funcall thunk)))

(defun +tasks--advance-here ()
  (org-back-to-heading t)
  (if-let* ((step (+tasks--next-step (org-get-todo-state))))
      (funcall step)
    (user-error "Nothing to advance at %s" (org-get-heading t t t t))))

(defun +tasks--advance-task (task)
  (with-current-buffer (plist-get task :buffer)
    (save-excursion
      (goto-char (plist-get task :marker))
      (+tasks--advance-here))))

(defun +tasks--here-is-a-task-p ()
  (or (+tasks--row-marker)
      (and (derived-mode-p 'org-mode) (+tasks--tasks-file-p buffer-file-name))))

(defun +tasks/next ()
  "Do what moves the task along: start, approve, test, review, finish, unpark or archive.
The task at point in tasks.org or on the Board; elsewhere, the one on the current branch, or picked."
  (interactive)
  (if (+tasks--here-is-a-task-p)
      (+tasks--at-heading-of-row #'+tasks--advance-here)
    (+tasks--advance-task (+tasks--work-task))))

;;; review, test, done

(defun +tasks--task-plist ()
  (list :buffer (current-buffer)
        :marker (point-marker)
        :heading (org-get-heading t t t t)
        :branch (org-entry-get nil "BRANCH")
        :accept (org-entry-get nil "ACCEPT")
        :state (org-get-todo-state)
        :parents (+tasks--task-parents)
        :model (org-entry-get nil "MODEL")
        :plan-model (org-entry-get nil "PLAN_MODEL")))

(defun +tasks--linked-task-at-point ()
  (when (and (+tasks--tasks-file-p buffer-file-name)
             (derived-mode-p 'org-mode)
             (not (org-before-first-heading-p)))
    (save-excursion
      (org-back-to-heading t)
      (when (org-entry-get nil "BRANCH")
        (+tasks--task-plist)))))

(defun +tasks--task-at (pos)
  (save-excursion
    (goto-char pos)
    (+tasks--task-plist)))

(defun +tasks--task-on-row-at-point ()
  (when-let* ((branch (and (+tasks--tasks-file-p buffer-file-name)
                           (+tasks--row-branch (thing-at-point 'line t))))
              (task (+tasks--branch-task branch)))
    (+tasks--task-at (nth 2 task))))

(defun +tasks--task-on-current-branch ()
  (when-let* ((branch (car (+tasks--git-lines "branch" "--show-current"))))
    (with-current-buffer (+tasks--tasks-buffer)
      (when-let* ((task (+tasks--branch-task branch)))
        (+tasks--task-at (nth 2 task))))))

(defun +tasks--linked-p (entry)
  (save-excursion
    (goto-char (cdr entry))
    (org-entry-get nil "BRANCH")))

(defun +tasks--pick-linked-task ()
  (with-current-buffer (+tasks--tasks-buffer)
    (let ((tasks (seq-filter #'+tasks--linked-p (+tasks--open-tasks org-not-done-keywords))))
      (unless tasks
        (user-error "No open task with a :BRANCH: in %s" buffer-file-name))
      (+tasks--task-at (cdr (assoc (completing-read "Task: " tasks nil t) tasks))))))

(defun +tasks--work-task ()
  "The task to review, test or finish: at point, linked to the branches row at point,
on the current branch, or picked."
  (or (+tasks--linked-task-at-point)
      (+tasks--task-on-row-at-point)
      (+tasks--task-on-current-branch)
      (+tasks--pick-linked-task)))

(defun +tasks--task-root (task)
  (buffer-local-value 'default-directory (plist-get task :buffer)))

(defun +tasks--range (main branch)
  (format "%s...%s" main branch))

;;; guard on diffs that widen what agents may do

(defvar +tasks-guarded-files
  (list (rx bos (or "modules/my/ai/" "modules/my/tasks/" ".pi/" ".agents/"))
        (rx (or bos "/")
            (or "AGENTS.md" "AGENTS.override.md" "CLAUDE.md" ".justfile" "justfile"
                (seq ".dir-locals" (? "-2") ".el"))
            eos))
  "Regexps over repo-relative paths, matched in any case, whose change can widen what agents may do.
The jail, the trust review, this flow and agent inputs pi loads as a
directory live in the first group; context files and files Emacs or the test
run act on with your rights, in the second.")

(defun +tasks--guarded-p (file)
  (let ((case-fold-search t))
    (and (seq-some (lambda (regexp) (string-match-p regexp file)) +tasks-guarded-files) t)))

(defun +tasks--guard-note (files)
  (when-let* ((guarded (seq-filter #'+tasks--guarded-p files)))
    (concat "Touches agent guards: " (string-join guarded ", "))))

(defun +tasks--changed-files (main branch)
  (+tasks--git-lines "diff" "--name-only" (+tasks--range main branch)))

(defun +tasks--confirm-guarded (branch files)
  (when-let* ((guarded (seq-filter #'+tasks--guarded-p files)))
    (unless (yes-or-no-p (format "%s changes agent guards (%s); merge anyway? "
                                 branch (string-join guarded ", ")))
      (user-error "Stopped before merging %s" branch))))

(defun +tasks/resume ()
  "Open a started task's worktree and focus its pi session."
  (interactive)
  (+tasks--resume-task (+tasks--work-task)))

(defun +tasks--resume-task (task &optional interview)
  "Open TASK's worktree and session; INTERVIEW drafts the grilling prompt even into a running session."
  (let* ((branch (plist-get task :branch))
         (root (file-name-as-directory (file-truename (+tasks--task-root task))))
         (worktree (+tasks--task-worktree root branch)))
    (+tasks--hand-off worktree
                      :review-root root
                      :draft (+tasks--resume-draft task (+tasks--plan-text (+tasks--task-worktree-of task))
                                                   worktree branch interview)
                      :prepare (lambda () (+tasks--ensure-skill root (+tasks--add-worktree root branch)))
                      :session branch
                      :model (+tasks--session-model task (+tasks--plan-text (+tasks--task-worktree-of task))))))

(defun +tasks/review ()
  "Show the task branch's whole diff against the default branch, with its ACCEPT."
  (interactive)
  (let* ((task (+tasks--work-task))
         (default-directory (+tasks--task-root task))
         (main (+tasks--default-branch))
         (branch (plist-get task :branch)))
    (magit-diff-range (+tasks--range main branch))
    (+tasks--set-state task "REVIEW")
    (message "%s" (+tasks--review-message (plist-get task :accept)
                                          (+tasks--guard-note (+tasks--changed-files main branch))
                                          (+tasks--plan-text (+tasks--task-worktree-of task))))))

(defun +tasks--review-message (accept guard-note &optional plan)
  (string-join (delq nil (list (format "ACCEPT: %s" (or accept "(none set)"))
                               guard-note
                               (+tasks--plan-tick-line plan)))
               "\n"))

(defun +tasks--review-question (plan)
  (if plan
      "Review this diff against them: what does not serve the criterion, what departs from the plan without saying so, missing or weak tests, risky changes."
    "Review this diff against it: what does not serve the criterion, missing or weak tests, risky changes."))

(defun +tasks--review-prompt (accept diff &optional plan)
  (concat (format "Acceptance criterion: %s\n\n" (or accept "(none set)"))
          (if plan (format "Plan the agent committed to:\n\n%s\n\n" (string-trim-right plan)) "")
          (+tasks--review-question plan)
          "\n\n" diff))

(defun +tasks--review-header (branch model guard-note)
  (concat (if guard-note (format "%s\n\n" guard-note) "")
          (format "Reviewing %s on %s...\n\n" branch model)))

(defun +tasks--review-buffer (branch guard-note)
  (with-current-buffer (get-buffer-create (format "*tasks-review:%s*" branch))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (+tasks--review-header branch (or (bound-and-true-p +ai-fast-model) "gptel") guard-note)))
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
         (main (+tasks--default-branch))
         (diff (string-join (+tasks--git-lines "diff" (+tasks--range main branch)) "\n"))
         (buffer (+tasks--review-buffer branch (+tasks--guard-note (+tasks--changed-files main branch))))
         (plan (+tasks--plan-text (+tasks--task-worktree-of task)))
         (gptel-model (or (bound-and-true-p +ai-fast-model) gptel-model)))
    (+tasks--set-state task "REVIEW")
    (pop-to-buffer buffer)
    (gptel-request (+tasks--review-prompt (plist-get task :accept) diff plan)
      :system (alist-get 'review gptel-directives)
      :callback (+tasks--review-callback buffer))))

(defun +tasks--test-command (justfile-text)
  (when (and justfile-text
             (string-match-p "^@?test\\(?:[ \t][^:\n]*\\)?:\\(?:[^=]\\|$\\)" justfile-text))
    "just test"))

(defun +tasks--justfile (root)
  "ROOT's justfile, named in any case, as just finds it."
  (let ((case-fold-search t))
    (and (file-directory-p root)
         (seq-find (lambda (file) (string-match-p "\\`\\.?justfile\\'" (file-name-nondirectory file)))
                   (directory-files root t directory-files-no-dot-files-regexp)))))

(defun +tasks--justfile-text (root)
  (when-let* ((file (+tasks--justfile root)))
    (+tasks--read-file file)))

(defun +tasks--choose-test-command (recipe-p just-p projectile-command)
  (if (and recipe-p just-p) "just test" projectile-command))

(defun +tasks--projectile-test-command (root)
  "Projectile's test command for ROOT, its dir-locals applied, whatever buffer is current."
  (when (fboundp 'projectile-test-command)
    (with-temp-buffer
      (setq default-directory root)
      (hack-dir-local-variables-non-file-buffer)
      (let ((command (ignore-errors (projectile-test-command root))))
        (and (stringp command) command)))))

(defun +tasks--warn-missing-just (recipe-p just-p)
  (when (and recipe-p (not just-p))
    (message "justfile has a test recipe but `just' is not on exec-path; using projectile's test command")))

(defun +tasks--project-test-command (root)
  (let ((recipe-p (+tasks--test-command (+tasks--justfile-text root)))
        (just-p (executable-find "just")))
    (+tasks--warn-missing-just recipe-p just-p)
    (+tasks--choose-test-command recipe-p just-p (+tasks--projectile-test-command root))))

(defun +tasks--test-argv (root command)
  "COMMAND as a shell call, in the agent jail when it applies: the tests are agent code."
  (let ((argv (list shell-file-name shell-command-switch command)))
    (if (fboundp '+ai--jailed) (+ai--jailed root argv) argv)))

(defun +tasks--shell-line (argv)
  (mapconcat #'shell-quote-argument argv " "))

(defun +tasks--task-worktree-of (task)
  (+tasks--branch-worktree-in (+tasks--task-root task) (plist-get task :branch)))

(defun +tasks--test-root (&optional task)
  "TASK's worktree, by default the task at point in tasks.org, else the current project."
  (or (when-let* ((task (or task (+tasks--linked-task-at-point))))
        (+tasks--task-worktree-of task))
      (+tasks--project-root)))

(defun +tasks--compile-tests (task root command)
  (with-current-buffer (compile (+tasks--shell-line (+tasks--test-argv root command)))
    (setq +tasks--compiled-task task)))

(defun +tasks/test ()
  "Run the tests of the task at point, or of this project, in the agent jail.
A task's run leaves it GREEN or RED when it finishes."
  (interactive)
  (let* ((task (+tasks--linked-task-at-point))
         (default-directory (+tasks--test-root task))
         (command (+tasks--project-test-command default-directory)))
    (if command
        (+tasks--compile-tests task default-directory command)
      (call-interactively #'projectile-test-project))))

(defun +tasks--merge-message (branch heading)
  (format "Merge %s: %s" branch heading))

(defun +tasks--dirty-p ()
  (+tasks--git-lines "status" "--porcelain" "--untracked-files=no"))

(defun +tasks--switch (branch)
  (unless (+tasks--git-ok-p "switch" branch)
    (user-error "git could not switch to %s" branch)))

(defun +tasks--run-tests (root command)
  (let ((default-directory root)
        (buffer (get-buffer-create "*tasks-done-tests*"))
        (argv (+tasks--test-argv root command)))
    (with-current-buffer buffer (erase-buffer))
    (message "Running %s in %s..." command root)
    (unless (zerop (apply #'call-process (car argv) nil buffer nil (cdr argv)))
      (display-buffer buffer)
      (user-error "%s failed; nothing merged" command))))

(defun +tasks--run-tests-or-confirm (root)
  "Run ROOT's tests there, or ask to go on without any."
  (if-let* ((command (+tasks--project-test-command root)))
      (+tasks--run-tests root command)
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

(defun +tasks--same-dir-p (a b)
  (equal (file-name-as-directory (file-truename a)) (file-name-as-directory (file-truename b))))

(defun +tasks--own-worktree (root branch)
  "BRANCH's worktree, unless it is checked out in ROOT itself (the old flow) or nowhere."
  (when-let* ((tree (+tasks--branch-worktree-in root branch)))
    (unless (+tasks--same-dir-p tree root)
      tree)))

(defun +tasks--worktree-dirty-p (tree)
  (let ((default-directory tree))
    (+tasks--git-lines "status" "--porcelain")))

(defun +tasks--test-branch (root branch tree)
  "Test BRANCH in its worktree TREE, or checked out in ROOT when it has none."
  (if tree
      (+tasks--run-tests-or-confirm tree)
    (+tasks--switch branch)
    (+tasks--run-tests-or-confirm root)))

(defun +tasks--remove-worktree (tree)
  (unless (+tasks--git-ok-p "worktree" "remove" tree)
    (user-error "Merged, but git could not remove %s; run git worktree remove there" tree)))

(defun +tasks--mark-done (branch buffer)
  (with-current-buffer buffer
    (goto-char (nth 2 (+tasks--branch-task branch)))
    (org-todo "DONE")
    (+tasks/refresh-branches)
    (save-buffer)))

(defmacro +tasks--or-set-state (task state &rest body)
  "Run BODY; should it stop with a user error, leave TASK in STATE, the error as the reason."
  (declare (indent 2))
  `(condition-case err
       (progn ,@body)
     (user-error (+tasks--set-state ,task ,state :reason (error-message-string err))
                 (signal (car err) (cdr err)))))

(defun +tasks/done (&optional despite-open-items)
  "Test the task's branch in its worktree, merge it --no-ff, remove both and mark the task DONE.
Refuses while the task's PLAN.md lists open items, unless DESPITE-OPEN-ITEMS (\\[universal-argument])."
  (interactive "P")
  (let* ((task (+tasks--work-task))
         (branch (plist-get task :branch))
         (default-directory (+tasks--task-root task))
         (main (+tasks--default-branch))
         (tree (+tasks--own-worktree default-directory branch))
         (plan (+tasks--plan-text tree)))
    (when (+tasks--dirty-p)
      (user-error "Commit or stash your changes first"))
    (when (and tree (+tasks--worktree-dirty-p tree))
      (user-error "Commit or remove the uncommitted work in %s first" tree))
    (unless despite-open-items
      (+tasks--refuse-open-items plan))
    (+tasks--confirm-guarded branch (+tasks--changed-files main branch))
    (+tasks--or-set-state task "RED"
      (+tasks--test-branch default-directory branch tree))
    (+tasks--or-set-state task "WAIT"
      (+tasks--merge branch main (plist-get task :heading)))
    (when tree
      (+tasks--remove-worktree tree))
    (+tasks--delete-branch branch)
    (+tasks--mark-done branch (plist-get task :buffer))
    (when (fboundp 'magit-refresh-all)
      (magit-refresh-all))
    (message "%s merged into %s; task done%s%s" branch main
             (if-let* ((ticks (+tasks--plan-tick-line plan))) (concat "; " ticks) "")
             (if tree "; quit its pi session, its worktree is gone" ""))))

;;; abort: throw the agent's work away, put the task back where it was

(defconst +tasks--started-log-regexp
  "^[ \t]*- State \"\\(?:PLAN\\|TODO\\)\"[ \t]+from \"\\([A-Z]+\\)\""
  "The log line a start writes; group 1 is the state the task came from.")

(defun +tasks--state-before-start ()
  "The state this task had before it was started, by its log; TODO when the log is silent."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t) (point)))
          (from nil))
      (while (re-search-forward +tasks--started-log-regexp end t)
        (setq from (match-string-no-properties 1)))
      (or from "TODO"))))

(defun +tasks--abort-entry ()
  "Unlink the task at point from its branch and return it to the state it was started from."
  (org-back-to-heading t)
  (let ((state (+tasks--state-before-start)))
    (org-entry-delete nil "BRANCH")
    (org-entry-delete nil "REASON")
    (org-todo state)))

(defun +tasks--stop-session (tree branch)
  (when (and tree (fboundp '+ai--stop-pilish-session))
    (+ai--stop-pilish-session tree branch)))

(defun +tasks--drop-worktree (tree)
  (unless (+tasks--git-ok-p "worktree" "remove" "--force" tree)
    (user-error "git could not remove %s" tree)))

(defun +tasks--backup-ref (branch)
  (concat "refs/aborted/" branch))

(defun +tasks--drop-branch (branch)
  "Delete BRANCH, keeping its tip under refs/aborted/ so the dropped work can still be read."
  (+tasks--git-ok-p "update-ref" (+tasks--backup-ref branch) branch)
  (unless (+tasks--git-ok-p "branch" "-D" branch)
    (user-error "git could not delete %s" branch)))

(defun +tasks--abort-question (task branch main)
  (format "Abort \"%s\": throw away %s (%d commit%s ahead of %s) and its worktree? "
          (plist-get task :heading) branch (+tasks--ahead branch main)
          (if (= 1 (+tasks--ahead branch main)) "" "s") main))

(defun +tasks/abort ()
  "Drop the task's branch, worktree and pi session, and put it back as it was before start.
Keeps ACCEPT and MODEL. Refuses a branch main already contains; that is done's job."
  (interactive)
  (let* ((task (+tasks--work-task))
         (branch (plist-get task :branch))
         (default-directory (+tasks--task-root task))
         (main (+tasks--default-branch))
         (tree (+tasks--own-worktree default-directory branch)))
    (when (+tasks--merged-p branch main)
      (user-error "%s is already merged into %s; finish it with done instead" branch main))
    (unless (yes-or-no-p (+tasks--abort-question task branch main))
      (user-error "Kept %s" branch))
    (+tasks--stop-session tree branch)
    (when tree
      (+tasks--drop-worktree tree))
    (+tasks--drop-branch branch)
    (with-current-buffer (plist-get task :buffer)
      (save-excursion
        (goto-char (plist-get task :marker))
        (+tasks--abort-entry))
      (+tasks/refresh-branches)
      (when buffer-file-name (save-buffer)))
    (when (fboundp 'magit-refresh-all)
      (magit-refresh-all))
    (message "%s dropped (tip kept at %s); task back to %s" branch (+tasks--backup-ref branch)
             (with-current-buffer (plist-get task :buffer)
               (save-excursion (goto-char (plist-get task :marker)) (org-get-todo-state))))))

;;; tasks.org buffers

(defun +tasks--refresh-on-save ()
  (condition-case err
      (+tasks/refresh-branches)
    (error (message "branches block not refreshed: %s" (error-message-string err)))))

(defun +tasks--block-parents (arg)
  "With ARG > 0, keep a task open while it has open sub-tasks, in this buffer only."
  (if (> arg 0)
      (progn
        (setq-local org-enforce-todo-dependencies t)
        (add-hook 'org-blocker-hook #'org-block-todo-from-children-or-siblings-or-parent nil t))
    (kill-local-variable 'org-enforce-todo-dependencies)
    (remove-hook 'org-blocker-hook #'org-block-todo-from-children-or-siblings-or-parent t)))

(define-minor-mode +tasks-file-mode
  "Branch refresh on save, parent blocking and the tasks menu for a project's tasks.org."
  :keymap (make-sparse-keymap)
  (if +tasks-file-mode
      (progn
        (add-hook 'before-save-hook #'+tasks--refresh-on-save nil t)
        (add-hook 'org-after-todo-state-change-hook #'+tasks--ask-wait-reason nil t)
        (+tasks--block-parents 1))
    (remove-hook 'before-save-hook #'+tasks--refresh-on-save t)
    (remove-hook 'org-after-todo-state-change-hook #'+tasks--ask-wait-reason t)
    (+tasks--block-parents -1)))

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
      :desc "tasks menu" "w" #'+tasks/menu
      :desc "next stage" "n" #'+tasks/next)

(map! :map org-agenda-mode-map
      :localleader
      :desc "next stage" "n" #'+tasks/next)
