;;; my/tasks/worktree.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +tasks--worktrees-dir-regexp ()
  (concat "[/\\\\]" (regexp-quote +tasks-worktree-dir) "\\'"))

;;;###autoload
(defun +tasks--worktree-path (root branch)
  (file-name-as-directory
   (expand-file-name (string-replace "/" "-" branch) (expand-file-name +tasks-worktree-dir root))))

;;;###autoload
(defun +tasks--parse-worktrees (lines)
  "(BRANCH . DIR) for each worktree in `git worktree list --porcelain' LINES."
  (let (dir result)
    (dolist (line lines (nreverse result))
      (cond ((string-prefix-p "worktree " line)
             (setq dir (file-name-as-directory (substring line (length "worktree ")))))
            ((string-prefix-p "branch refs/heads/" line)
             (push (cons (substring line (length "branch refs/heads/")) dir) result))))))

;;;###autoload
(defun +tasks--branch-worktree-in (root branch)
  (let ((default-directory root))
    (cdr (assoc branch (+tasks--parse-worktrees (+git-lines "worktree" "list" "--porcelain"))))))

;;;###autoload
(defun +tasks--task-worktree (root branch)
  "Where BRANCH's work happens: the worktree it is checked out in, else the one start will add."
  (or (+tasks--branch-worktree-in root branch) (+tasks--worktree-path root branch)))

;;;###autoload
(defun +tasks--worktree-add-args (tree branch)
  (if (+git-ok-p "show-ref" "--verify" "--quiet" (concat "refs/heads/" branch))
      (list tree branch)
    (list "-b" branch tree)))

;;;###autoload
(defun +tasks--add-worktree (root branch)
  "The worktree BRANCH is checked out in, adding one under ROOT's `+tasks-worktree-dir' if none."
  (let ((default-directory root))
    (or (+tasks--branch-worktree-in root branch)
        (let ((tree (+tasks--worktree-path root branch)))
          (+tasks--exclude-from-git)
          (unless (apply #'+git-ok-p "worktree" "add" "--quiet" (+tasks--worktree-add-args tree branch))
            (user-error "git could not add a worktree for %s at %s" branch tree))
          (when (fboundp 'magit-refresh-all)
            (magit-refresh-all))
          tree))))

;;;###autoload
(defun +tasks--task-worktree-of (task)
  (+tasks--branch-worktree-in (+tasks--task-root task) (plist-get task :branch)))

;;;###autoload
(defun +tasks--same-dir-p (a b)
  (equal (file-name-as-directory (file-truename a)) (file-name-as-directory (file-truename b))))

;;;###autoload
(defun +tasks--own-worktree (root branch)
  "BRANCH's worktree, unless it is checked out in ROOT itself (the old flow) or nowhere."
  (when-let* ((tree (+tasks--branch-worktree-in root branch)))
    (unless (+tasks--same-dir-p tree root)
      tree)))

;;;###autoload
(defun +tasks--worktree-dirty-p (tree)
  (let ((default-directory tree))
    (+git-lines "status" "--porcelain")))

;;;###autoload
(defun +tasks--remove-worktree (tree)
  (unless (+git-ok-p "worktree" "remove" tree)
    (user-error "Merged, but git could not remove %s; run git worktree remove there" tree)))

;;;###autoload
(defun +tasks--drop-worktree (tree)
  (unless (+git-ok-p "worktree" "remove" "--force" tree)
    (user-error "git could not remove %s" tree)))