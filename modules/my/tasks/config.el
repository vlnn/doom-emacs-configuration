;;; my/tasks/config.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar gptel-model)

(defconst +tasks--dir (dir!))

(defvar +tasks-file-name "tasks.org")

(defvar +tasks-worktree-dir ".worktrees"
  "Where task worktrees live, relative to the repo's main checkout.")

(defvar +tasks-branch-prefixes '("feat/")
  "Git branch prefixes the branches block treats as tracked work.")

(defconst +tasks-plan-file-name "PLAN.md")

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

(defvar +tasks-agent nil
  "How tasks reach the coding agent; the module that provides one sets it (ai's pi.el).
A plist of functions: :launch (root &rest keys), :send (line &optional session),
:prefill (draft &optional session), :input-buffer (&optional session),
:stop (root &optional session), :live-p (root &optional session),
:jail (root command &optional network), :check-model (model), :models (),
:review-model (). A missing key means no agent: drafts go to the kill ring,
tests run unjailed, any :MODEL: passes.")

(defun +tasks--agent (key)
  (plist-get +tasks-agent key))

(defun +tasks--agent-call (key &rest args)
  "Call the agent's KEY function with ARGS; nil when there is none."
  (when-let* ((fn (+tasks--agent key)))
    (apply fn args)))


;;; knobs the autoload files read

(defvar +tasks-board-scope 'all
  "Which projects the board shows: `all', or `project' for the one it is opened from.")

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

;;; what runs at startup for the autoload files

(after! org-agenda
  (+tasks--install-agenda))

(after! lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories (+tasks--worktrees-dir-regexp)))

(add-hook 'compilation-finish-functions #'+tasks--mark-compiled-tests)

(defun +tasks--string-list-p (value)
  (and (listp value) (seq-every-p #'stringp value) t))

(put '+tasks-branch-prefixes 'safe-local-variable #'+tasks--string-list-p)

;;; capture and init

(defun +tasks--existing-project-file ()
  (let ((file (+tasks--project-file (+main-checkout (+project-root)))))
    (unless (file-exists-p file)
      (user-error "No %s in this project; run M-x +tasks/init" +tasks-file-name))
    file))

(defun +tasks/init ()
  "Write the tasks.org skeleton at the project root and open it.
Offers the grilling skill and an AGENTS.md too, asking before each."
  (interactive)
  (let* ((root (+main-checkout (+project-root)))
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
  (when-let* ((path (car (+git-lines "rev-parse" "--git-path" "info/exclude"))))
    (expand-file-name path)))

(defun +tasks--exclude-from-git ()
  "Keep tasks.org and its archive out of git via .git/info/exclude."
  (when-let* ((file (+tasks--exclude-file)))
    (let ((missing (+tasks--exclude-lines (if (file-exists-p file) (+read-file file) ""))))
      (when missing
        (make-directory (file-name-directory file) t)
        (write-region (concat (mapconcat #'identity missing "\n") "\n") nil file t)))))

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

(cl-defun +tasks--hand-off (root &key draft send prepare session model review-root)
  "Give a task to pi in ROOT; PREPARE runs only once REVIEW-ROOT's review allows it."
  (if (+tasks--agent :launch)
      (+tasks--agent-call :launch root :draft draft :send send :prepare prepare :session session :model model
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
