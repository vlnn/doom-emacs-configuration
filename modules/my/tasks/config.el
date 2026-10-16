;;; my/tasks/config.el -*- lexical-binding: t; -*-

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
  (when (equal branch (org-entry-get nil "BRANCH"))
    (org-get-heading t t t t)))

(defun +tasks--task-for-branch (branch)
  (save-restriction
    (widen)
    (car (delq nil (org-map-entries (lambda () (+tasks--heading-if-on-branch branch)))))))

(defun +tasks--branch-row (branch age task)
  (format "| %s | %s | %s |\n" branch age (or task "NONE")))

(defun org-dblock-write:branches (_params)
  (insert "| Branch | Last commit | Task |\n|-\n")
  (pcase-dolist (`(,branch . ,age) (+tasks--git-branches +tasks-branch-prefixes))
    (insert (+tasks--branch-row branch age (+tasks--task-for-branch branch))))
  (org-table-align))

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
