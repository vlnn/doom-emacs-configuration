;;; my/tasks/branches.el -*- lexical-binding: t; -*-

;;; branches dynamic block

;;;###autoload
(defun +tasks--default-branch-from (origin-head locals)
  (cond (origin-head (string-remove-prefix "refs/remotes/origin/" origin-head))
        ((member "main" locals) "main")
        ((member "master" locals) "master")
        (t "main")))

;;;###autoload
(defun +tasks--default-branch ()
  (+tasks--default-branch-from
   (car (+git-lines "symbolic-ref" "-q" "refs/remotes/origin/HEAD"))
   (+git-lines "branch" "--format=%(refname:short)")))

;;;###autoload
(defun +tasks--merged-p (branch main)
  (or (+git-ok-p "merge-base" "--is-ancestor" branch main)
      (+git-ok-p "merge-base" "--is-ancestor" branch (concat "origin/" main))))

;;;###autoload
(defun +tasks--ahead (branch main)
  (string-to-number (or (car (+git-lines "rev-list" "--count" (concat main ".." branch))) "0")))

;;;###autoload
(defun +tasks--branch-patterns (prefixes)
  (mapcar (lambda (prefix) (concat prefix "*")) prefixes))

;;;###autoload
(defun +tasks--parse-branch-line (line)
  (pcase-let ((`(,branch ,age) (split-string line "\t")))
    (cons branch age)))

;;;###autoload
(defun +tasks--git-branches (prefixes)
  (mapcar #'+tasks--parse-branch-line
          (apply #'process-lines "git" "branch" "--list"
                 "--format=%(refname:short)\t%(committerdate:relative)"
                 (+tasks--branch-patterns prefixes))))

;;;###autoload
(defun +tasks--task-if-on-branch (branch)
  (when-let* ((state (org-get-todo-state)))
    (when (equal branch (org-entry-get nil "BRANCH"))
      (list (org-get-heading t t t t) state (point)))))

;;;###autoload
(defun +tasks--branch-task (branch)
  "The task linked to BRANCH in the current buffer, as (HEADING STATE POS)."
  (save-restriction
    (widen)
    (car (delq nil (org-map-entries (lambda () (+tasks--task-if-on-branch branch)))))))

;;;###autoload
(defun +tasks--task-for-branch (branch)
  (car (+tasks--branch-task branch)))

;;;###autoload
(defun +tasks--state-cell (merged ahead task-state)
  (let ((progress (if merged "merged" (format "ahead %d" ahead))))
    (cond ((not (equal task-state "DONE")) progress)
          (merged "DONE! delete branch")
          (t (concat "DONE! " progress)))))

;;;###autoload
(defun +tasks--branch-row (branch age task state)
  (format "| %s | %s | %s | %s |" branch age (or task "NONE") state))

;;;###autoload
(defun +tasks--branch-table (rows)
  (string-join (cons "| Branch | Last commit | Task | State |\n|-" rows) "\n"))

;;;###autoload
(defun +tasks--branch-row-for (branch age main)
  (pcase-let ((`(,heading ,state ,_) (+tasks--branch-task branch)))
    (+tasks--branch-row branch age heading
                        (+tasks--state-cell (+tasks--merged-p branch main)
                                            (+tasks--ahead branch main)
                                            state))))

;;;###autoload
(defun +tasks--branch-rows (prefixes)
  (let ((main (+tasks--default-branch)))
    (mapcar (pcase-lambda (`(,branch . ,age)) (+tasks--branch-row-for branch age main))
            (+tasks--git-branches prefixes))))

;;;###autoload
(defun +tasks--aligned-table (rows)
  (with-temp-buffer
    (insert (+tasks--branch-table rows))
    (delay-mode-hooks (org-mode))
    (goto-char (point-min))
    (org-table-align)
    (string-trim-right (buffer-string))))

;;;###autoload
(defun +tasks--current-table ()
  (+tasks--aligned-table (+tasks--branch-rows +tasks-branch-prefixes)))

;;;###autoload
(defun org-dblock-write:branches (_params)
  (insert (+tasks--current-table)))

;;;###autoload
(defun +tasks--row-branch (line)
  (when (string-match "\\`|[[:blank:]]*\\([^|[:blank:]]+\\)[[:blank:]]*|" line)
    (let ((cell (match-string 1 line)))
      (unless (or (equal cell "Branch") (string-prefix-p "-" cell))
        cell))))

;;;###autoload
(defun +tasks--block-body-bounds ()
  "Bounds of the branches block body, rows plus their final newline."
  (goto-char (point-min))
  (when (re-search-forward "^[ \t]*#\\+BEGIN: branches.*\n" nil t)
    (let ((start (point)))
      (when (re-search-forward "^[ \t]*#\\+END:" nil t)
        (cons start (match-beginning 0))))))

;;;###autoload
(defun +tasks--replace-body (bounds text)
  "Swap in TEXT and drop the element cache, which misparses large table rewrites."
  (unless (equal text (buffer-substring-no-properties (car bounds) (cdr bounds)))
    (goto-char (car bounds))
    (delete-region (car bounds) (cdr bounds))
    (insert text)
    (org-element-cache-reset)))

;;;###autoload
(defun +tasks/refresh-branches ()
  "Update the branches block of the current tasks.org."
  (interactive)
  (save-excursion
    (when-let* ((bounds (+tasks--block-body-bounds)))
      (+tasks--replace-body bounds (concat (+tasks--current-table) "\n")))))

;;;###autoload
(defun +tasks--strip-prefix (branch prefixes)
  (if-let* ((prefix (seq-find (lambda (prefix) (string-prefix-p prefix branch)) prefixes)))
      (substring branch (length prefix))
    branch))

;;;###autoload
(defun +tasks--branch-title (branch prefixes)
  (string-replace "-" " " (+tasks--strip-prefix branch prefixes)))

;;;###autoload
(defun +tasks--insert-idea (title branch)
  (goto-char (+tasks--top-heading-end "Ideas"))
  (insert "** IDEA " title "\n")
  (forward-line -1)
  (org-entry-put nil "BRANCH" branch))

;;;###autoload
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