;;; my/tasks/agenda.el -*- lexical-binding: t; -*-

(defvar +tasks--added-agenda-files nil
  "The tasks.org files the last refresh added to `org-agenda-files'.")

;;; agenda files

;;;###autoload
(defun +tasks--project-file (root)
  (expand-file-name +tasks-file-name root))

;;;###autoload
(defun +tasks--project-files (roots)
  (seq-uniq (mapcar #'file-truename
                    (seq-filter #'file-exists-p
                                (mapcar #'+tasks--project-file (seq-remove #'file-remote-p roots))))))

;;;###autoload
(defun +tasks--agenda-files (base roots)
  (seq-uniq (append base (+tasks--project-files roots))))

;;;###autoload
(defun +tasks--agenda-base (current added)
  "CURRENT `org-agenda-files' as a list, less the tasks.org files a refresh ADDED to it.
Empty, it is `org-directory', as Doom's default would have it."
  (or (seq-difference (if (listp current) current (org-agenda-files t)) added)
      (list org-directory)))

;;;###autoload
(defun +tasks--refresh-agenda-files (&rest _)
  "Add each known project's tasks.org to `org-agenda-files', keeping whatever else it holds."
  (let ((base (+tasks--agenda-base org-agenda-files +tasks--added-agenda-files)))
    (setq org-agenda-files (+tasks--agenda-files base (bound-and-true-p projectile-known-projects))
          +tasks--added-agenda-files (seq-difference org-agenda-files base))
    org-agenda-files))

;;;###autoload
(defun +tasks--advise-agenda-commands ()
  (dolist (command +tasks-agenda-commands)
    (advice-add command :before #'+tasks--refresh-agenda-files)))

(defconst +tasks--board-prefix-width 24)

(defconst +tasks--board-prefix-format
  `((todo . ,(format " %%-12:c %%-%d(+tasks--agenda-prefix) " +tasks--board-prefix-width))))

;;;###autoload
(defun +tasks--agenda-prefix ()
  "What the board shows before a task: why it waits, what its plan needs, else its branch, else nothing."
  (truncate-string-to-width (or (and (equal (org-get-todo-state) "WAIT") (org-entry-get nil "REASON"))
                                (+tasks--plan-mark-at-point)
                                (org-entry-get nil "BRANCH")
                                "")
                            +tasks--board-prefix-width nil nil "…"))

;;;###autoload
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

;;;###autoload
(defun +tasks--with-agenda-commands (commands ours)
  "COMMANDS plus OURS, which replace any command under the same key."
  (append (seq-remove (lambda (command) (assoc (car command) ours)) commands) ours))

;;;###autoload
(defun +tasks--install-agenda ()
  "Refresh agenda files before each agenda command and add the Board, Last week and review views."
  (+tasks--advise-agenda-commands)
  (setq org-agenda-custom-commands
        (+tasks--with-agenda-commands org-agenda-custom-commands +tasks--agenda-custom-commands)))

;;;###autoload
(defun +tasks--project-filter (root)
  (list (concat "+" (+tasks--project-category (+tasks--project-file root)))))

;;;###autoload
(defun +tasks--board-filter (scope root)
  (when (eq scope 'project)
    (+tasks--project-filter root)))

;;;###autoload
(defun +tasks/board ()
  "Open the Board agenda view, narrowed to this project when `+tasks-board-scope' says so."
  (interactive)
  (let ((filter (+tasks--board-filter +tasks-board-scope (+project-root))))
    (org-agenda nil "b")
    (org-agenda-filter-show-all-cat)
    (when filter
      (org-agenda-filter-apply filter 'category))))

;;;###autoload
(defun +tasks/toggle-board-scope ()
  "Flip the board between this project's tasks and every project's, and show it."
  (interactive)
  (setq +tasks-board-scope (if (eq +tasks-board-scope 'all) 'project 'all))
  (+tasks/board))