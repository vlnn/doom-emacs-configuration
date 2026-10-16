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
  (seq-filter #'file-exists-p
              (mapcar #'+tasks--project-file (seq-remove #'file-remote-p roots))))

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

(defun +tasks--heading-if-on-branch (branch)
  (when (and (org-get-todo-state)
             (equal branch (org-entry-get nil "BRANCH")))
    (org-get-heading t t t t)))

(defun +tasks--task-for-branch (branch)
  (save-restriction
    (widen)
    (car (delq nil (org-map-entries (lambda () (+tasks--heading-if-on-branch branch)))))))

(defun +tasks--branch-row (branch age task)
  (format "| %s | %s | %s |" branch age (or task "NONE")))

(defun +tasks--branch-table (rows)
  (string-join (cons "| Branch | Last commit | Task |\n|-" rows) "\n"))

(defun +tasks--branch-rows (prefixes)
  (mapcar (pcase-lambda (`(,branch . ,age))
            (+tasks--branch-row branch age (+tasks--task-for-branch branch)))
          (+tasks--git-branches prefixes)))

(defun org-dblock-write:branches (_params)
  (insert (+tasks--branch-table (+tasks--branch-rows +tasks-branch-prefixes)))
  (org-table-align))

(defun +tasks--row-branch (line)
  (when (string-match "\\`|[[:blank:]]*\\([^|[:blank:]]+\\)[[:blank:]]*|" line)
    (let ((cell (match-string 1 line)))
      (unless (or (equal cell "Branch") (string-prefix-p "-" cell))
        cell))))

(defun +tasks/refresh-branches ()
  "Update the branches block of the current tasks.org."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^[ \t]*#\\+BEGIN: branches" nil t)
      (beginning-of-line)
      (org-update-dblock))))

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
    (find-file file)))

(defconst +tasks--capture-template
  '("pi" "Project idea" entry
    (file+headline +tasks--existing-project-file "Ideas")
    "* IDEA %?\n%i" :prepend t))

(after! org-capture
  (add-to-list 'org-capture-templates +tasks--capture-template t))

;;; starting work on a task

(defconst +tasks-slug-max 40)

(defconst +tasks-start-states '("IDEA" "TODO"))

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

(defun +tasks--feature-prompt (heading)
  (format "/feature %s" heading))

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

(defun +tasks--task-at-point ()
  (when (and (+tasks--tasks-file-p buffer-file-name)
             (derived-mode-p 'org-mode)
             (not (org-before-first-heading-p)))
    (save-excursion
      (org-back-to-heading t)
      (when (member (org-get-todo-state) +tasks-start-states)
        (cons (current-buffer) (point))))))

(defun +tasks--pick-task ()
  (let* ((buffer (+tasks--tasks-buffer))
         (tasks (with-current-buffer buffer (+tasks--open-tasks))))
    (unless tasks
      (user-error "No IDEA or TODO in %s" (buffer-file-name buffer)))
    (cons buffer (cdr (assoc (completing-read "Task: " tasks nil t) tasks)))))

(defun +tasks--chosen-task ()
  (or (+tasks--task-at-point) (+tasks--pick-task)))

(defun +tasks--read-prefix ()
  (if (cdr +tasks-branch-prefixes)
      (completing-read "Branch prefix: " +tasks-branch-prefixes nil t)
    (car +tasks-branch-prefixes)))

(defun +tasks--read-accept (heading)
  (let ((accept (string-trim (read-string (format "ACCEPT for \"%s\" (one testable line): " heading)))))
    (unless (string-empty-p accept) accept)))

(defun +tasks--git-ok-p (&rest args)
  (zerop (apply #'call-process "git" nil nil nil args)))

(defun +tasks--checkout-branch (branch)
  (unless (if (+tasks--git-ok-p "show-ref" "--verify" "--quiet" (concat "refs/heads/" branch))
              (+tasks--git-ok-p "switch" branch)
            (+tasks--git-ok-p "switch" "-c" branch))
    (user-error "git could not switch to %s; check uncommitted changes" branch))
  (when (fboundp 'magit-refresh-all)
    (magit-refresh-all)))

(defun +tasks--hand-off (heading)
  (let ((prompt (+tasks--feature-prompt heading)))
    (kill-new prompt)
    (if (fboundp '+ai/pi)
        (+ai/pi prompt)
      (message "Copied: %s" prompt))))

(defun +tasks/start ()
  "Branch, link, promote and refile a task, then open pi on it.
Uses the IDEA or TODO at point in tasks.org, otherwise asks for one."
  (interactive)
  (pcase-let ((`(,buffer . ,pos) (+tasks--chosen-task)))
    (with-current-buffer buffer
      (goto-char pos)
      (let* ((heading (org-get-heading t t t t))
             (branch (read-string "Branch: " (+tasks--branch-name (+tasks--read-prefix) heading)))
             (accept (unless (org-entry-get nil "ACCEPT") (+tasks--read-accept heading))))
        (+tasks--checkout-branch branch)
        (+tasks--start-entry branch accept)
        (+tasks/refresh-branches)
        (save-buffer)
        (+tasks--hand-off heading)))))

(defun +tasks/accept ()
  "Set the one-line acceptance criterion of a task."
  (interactive)
  (pcase-let ((`(,buffer . ,pos) (+tasks--chosen-task)))
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

(defun +tasks--enable-file-mode-h ()
  (when (+tasks--tasks-file-p buffer-file-name)
    (+tasks-file-mode 1)))

(add-hook! 'org-mode-hook #'+tasks--enable-file-mode-h)

(map! :leader :desc "tasks" "5" #'+tasks/menu)

(map! :map +tasks-file-mode-map
      :localleader
      :desc "tasks menu" "w" #'+tasks/menu)
