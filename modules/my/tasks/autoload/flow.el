;;; my/tasks/flow.el -*- lexical-binding: t; -*-

(declare-function magit-diff-range "magit-diff")

;;; stages

;;;###autoload
(defun +tasks--keep-reason (state reason)
  (if (equal state "WAIT")
      (when reason (org-entry-put nil "REASON" reason))
    (org-entry-delete nil "REASON")))

;;;###autoload
(defun +tasks--enter-state (state &optional reason)
  (org-back-to-heading t)
  (+tasks--keep-reason state reason)
  (org-todo state))

;;;###autoload
(cl-defun +tasks--set-state (task state &key reason)
  "Move TASK to STATE in its own buffer; REASON stays as :REASON: while it waits."
  (with-current-buffer (plist-get task :buffer)
    (save-excursion
      (goto-char (plist-get task :marker))
      (+tasks--enter-state state reason))
    (when buffer-file-name
      (save-buffer))))

;;;###autoload
(defun +tasks--ask-wait-reason ()
  "After a state change by hand: ask why a task waits, forget why once it moves on."
  (if (equal org-state "WAIT")
      (unless (org-entry-get nil "REASON")
        (org-entry-put nil "REASON" (read-string "Waiting on: ")))
    (org-entry-delete nil "REASON")))

;;;###autoload
(defun +tasks/wait ()
  "Park the task on something outside the repo, saying what."
  (interactive)
  (let ((task (+tasks--work-task)))
    (+tasks--set-state task "WAIT" :reason (read-string (format "%s waits on: " (plist-get task :heading))))))

;;;###autoload
(defun +tasks--test-verdict (compilation-message)
  (if (string-prefix-p "finished" compilation-message) "GREEN" "RED"))

(defvar-local +tasks--compiled-task nil
  "The task whose tests this compilation buffer runs.")

;;;###autoload
(defun +tasks--verdict-note (verdict plan)
  "VERDICT with the plan's tick count; a GREEN the plan does not vouch for is flagged."
  (pcase-let ((`(,ticked . ,total) (if plan (+tasks--plan-tick-count plan) '(0 . 0))))
    (cond ((null plan) verdict)
          ((and (equal verdict "GREEN") (< ticked total))
           (format "%s, but plan: %d/%d tests ticked" verdict ticked total))
          (t (format "%s; plan: %d/%d tests ticked" verdict ticked total)))))

;;;###autoload
(defun +tasks--mark-compiled-tests (buffer compilation-message)
  (when-let* ((task (buffer-local-value '+tasks--compiled-task buffer))
              ((buffer-live-p (plist-get task :buffer))))
    (let ((verdict (+tasks--test-verdict compilation-message)))
      (+tasks--set-state task verdict)
      (message "%s: %s" (plist-get task :heading)
               (+tasks--verdict-note verdict (+tasks--plan-text (+tasks--task-worktree-of task)))))))

;;; next: the one key that moves the task at point along

(defconst +tasks--next-steps
  '(("IDEA" . +tasks/start) ("TODO" . +tasks/start)
    ("PLAN" . +tasks/approve-plan) ("RED" . +tasks/test)
    ("GREEN" . +tasks/review) ("REVIEW" . +tasks/done)
    ("WAIT" . +tasks/unpark)
    ("DONE" . org-archive-subtree) ("DROPPED" . org-archive-subtree))
  "For each state, the command that takes a task to its next stage.")

;;;###autoload
(defun +tasks--next-step (state)
  (cdr (assoc state +tasks--next-steps)))

;;;###autoload
(defun +tasks/unpark ()
  "Take a waiting task back to RED, or to PLAN while its plan is not approved, and reopen its pi session."
  (interactive)
  (let ((task (+tasks--work-task)))
    (+tasks--set-state task (+tasks--unparked-state task)))
  (+tasks/resume))

;;;###autoload
(defun +tasks--row-marker ()
  (and (derived-mode-p 'org-agenda-mode)
       (or (org-get-at-bol 'org-hd-marker) (org-get-at-bol 'org-marker))))

;;;###autoload
(defun +tasks--at-heading-of-row (thunk)
  "Run THUNK in the org buffer at the heading a board row points to, or here."
  (if-let* ((marker (+tasks--row-marker)))
      (progn
        (pop-to-buffer (marker-buffer marker))
        (goto-char marker)
        (funcall thunk))
    (funcall thunk)))

;;;###autoload
(defun +tasks--advance-here ()
  (org-back-to-heading t)
  (if-let* ((step (+tasks--next-step (org-get-todo-state))))
      (funcall step)
    (user-error "Nothing to advance at %s" (org-get-heading t t t t))))

;;;###autoload
(defun +tasks--advance-task (task)
  (with-current-buffer (plist-get task :buffer)
    (save-excursion
      (goto-char (plist-get task :marker))
      (+tasks--advance-here))))

;;;###autoload
(defun +tasks--here-is-a-task-p ()
  (or (+tasks--row-marker)
      (and (derived-mode-p 'org-mode) (+tasks--tasks-file-p buffer-file-name))))

;;;###autoload
(defun +tasks/next ()
  "Do what moves the task along: start, approve, test, review, finish, unpark or archive.
The task at point in tasks.org or on the Board; elsewhere, the one on the current branch, or picked."
  (interactive)
  (if (+tasks--here-is-a-task-p)
      (+tasks--at-heading-of-row #'+tasks--advance-here)
    (+tasks--advance-task (+tasks--work-task))))

;;; review, test, done

;;;###autoload
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

;;;###autoload
(defun +tasks--linked-task-at-point ()
  (when (and (+tasks--tasks-file-p buffer-file-name)
             (derived-mode-p 'org-mode)
             (not (org-before-first-heading-p)))
    (save-excursion
      (org-back-to-heading t)
      (when (org-entry-get nil "BRANCH")
        (+tasks--task-plist)))))

;;;###autoload
(defun +tasks--task-at (pos)
  (save-excursion
    (goto-char pos)
    (+tasks--task-plist)))

;;;###autoload
(defun +tasks--task-on-row-at-point ()
  (when-let* ((branch (and (+tasks--tasks-file-p buffer-file-name)
                           (+tasks--row-branch (thing-at-point 'line t))))
              (task (+tasks--branch-task branch)))
    (+tasks--task-at (nth 2 task))))

;;;###autoload
(defun +tasks--task-on-current-branch ()
  (when-let* ((branch (car (+git-lines "branch" "--show-current"))))
    (with-current-buffer (+tasks--tasks-buffer)
      (when-let* ((task (+tasks--branch-task branch)))
        (+tasks--task-at (nth 2 task))))))

;;;###autoload
(defun +tasks--linked-p (entry)
  (save-excursion
    (goto-char (cdr entry))
    (org-entry-get nil "BRANCH")))

;;;###autoload
(defun +tasks--pick-linked-task ()
  (with-current-buffer (+tasks--tasks-buffer)
    (let ((tasks (seq-filter #'+tasks--linked-p (+tasks--open-tasks org-not-done-keywords))))
      (unless tasks
        (user-error "No open task with a :BRANCH: in %s" buffer-file-name))
      (+tasks--task-at (cdr (assoc (completing-read "Task: " tasks nil t) tasks))))))

;;;###autoload
(defun +tasks--work-task ()
  "The task to review, test or finish: at point, linked to the branches row at point,
on the current branch, or picked."
  (or (+tasks--linked-task-at-point)
      (+tasks--task-on-row-at-point)
      (+tasks--task-on-current-branch)
      (+tasks--pick-linked-task)))

;;;###autoload
(defun +tasks--task-root (task)
  (buffer-local-value 'default-directory (plist-get task :buffer)))

;;;###autoload
(defun +tasks--range (main branch)
  (format "%s...%s" main branch))

;;;###autoload
(defun +tasks--guarded-p (file)
  (let ((case-fold-search t))
    (and (seq-some (lambda (regexp) (string-match-p regexp file)) +tasks-guarded-files) t)))

;;;###autoload
(defun +tasks--guard-note (files)
  (when-let* ((guarded (seq-filter #'+tasks--guarded-p files)))
    (concat "Touches agent guards: " (string-join guarded ", "))))

;;;###autoload
(defun +tasks--changed-files (main branch)
  (+git-lines "diff" "--name-only" (+tasks--range main branch)))

;;;###autoload
(defun +tasks--confirm-guarded (branch files)
  (when-let* ((guarded (seq-filter #'+tasks--guarded-p files)))
    (unless (yes-or-no-p (format "%s changes agent guards (%s); merge anyway? "
                                 branch (string-join guarded ", ")))
      (user-error "Stopped before merging %s" branch))))

;;;###autoload
(defun +tasks/resume ()
  "Open a started task's worktree and focus its pi session."
  (interactive)
  (+tasks--resume-task (+tasks--work-task)))

;;;###autoload
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

;;;###autoload
(defun +tasks/review ()
  "Show the task branch's whole diff against the default branch, with its ACCEPT."
  (interactive)
  (let* ((task (+tasks--work-task))
         (default-directory (+tasks--task-root task))
         (main (+tasks--default-branch))
         (branch (plist-get task :branch)))
    (require 'magit)
    (magit-diff-range (+tasks--range main branch))
    (+tasks--set-state task "REVIEW")
    (message "%s" (+tasks--review-message (plist-get task :accept)
                                          (+tasks--guard-note (+tasks--changed-files main branch))
                                          (+tasks--plan-text (+tasks--task-worktree-of task))))))

;;;###autoload
(defun +tasks--review-message (accept guard-note &optional plan)
  (string-join (delq nil (list (format "ACCEPT: %s" (or accept "(none set)"))
                               guard-note
                               (+tasks--plan-tick-line plan)))
               "\n"))

;;;###autoload
(defun +tasks--review-question (plan)
  (if plan
      "Review this diff against them: what does not serve the criterion, what departs from the plan without saying so, missing or weak tests, risky changes."
    "Review this diff against it: what does not serve the criterion, missing or weak tests, risky changes."))

;;;###autoload
(defun +tasks--review-prompt (accept diff &optional plan)
  (concat (format "Acceptance criterion: %s\n\n" (or accept "(none set)"))
          (if plan (format "Plan the agent committed to:\n\n%s\n\n" (string-trim-right plan)) "")
          (+tasks--review-question plan)
          "\n\n" diff))

;;;###autoload
(defun +tasks--review-header (branch model guard-note)
  (concat (if guard-note (format "%s\n\n" guard-note) "")
          (format "Reviewing %s on %s...\n\n" branch model)))

;;;###autoload
(defun +tasks--review-buffer (branch guard-note)
  (with-current-buffer (get-buffer-create (format "*tasks-review:%s*" branch))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (+tasks--review-header branch (or (+tasks--agent-call :review-model) "gptel") guard-note)))
    (current-buffer)))

;;;###autoload
(defun +tasks--review-callback (buffer)
  (lambda (response info)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (if (stringp response)
                    response
                  (format "Review failed: %s" (plist-get info :status))))))))

;;;###autoload
(defun +tasks/ai-review ()
  "Ask the local model, fresh, to review the task branch against its ACCEPT."
  (interactive)
  (require 'gptel)
  (let* ((task (+tasks--work-task))
         (default-directory (+tasks--task-root task))
         (branch (plist-get task :branch))
         (main (+tasks--default-branch))
         (diff (string-join (+git-lines "diff" (+tasks--range main branch)) "\n"))
         (buffer (+tasks--review-buffer branch (+tasks--guard-note (+tasks--changed-files main branch))))
         (plan (+tasks--plan-text (+tasks--task-worktree-of task)))
         (gptel-model (or (+tasks--agent-call :review-model) gptel-model)))
    (+tasks--set-state task "REVIEW")
    (pop-to-buffer buffer)
    (gptel-request (+tasks--review-prompt (plist-get task :accept) diff plan)
      :system (alist-get 'review gptel-directives)
      :callback (+tasks--review-callback buffer))))

;;;###autoload
(defun +tasks--test-command (justfile-text)
  (when (and justfile-text
             (string-match-p "^@?test\\(?:[ \t][^:\n]*\\)?:\\(?:[^=]\\|$\\)" justfile-text))
    "just test"))

;;;###autoload
(defun +tasks--justfile-text (root)
  (when-let* ((file (car (+justfiles root))))
    (+read-file file)))

;;;###autoload
(defun +tasks--choose-test-command (recipe-p just-p projectile-command)
  (if (and recipe-p just-p) "just test" projectile-command))

;;;###autoload
(defun +tasks--projectile-test-command (root)
  "Projectile's test command for ROOT, its dir-locals applied, whatever buffer is current."
  (when (fboundp 'projectile-test-command)
    (with-temp-buffer
      (setq default-directory root)
      (hack-dir-local-variables-non-file-buffer)
      (let ((command (ignore-errors (projectile-test-command root))))
        (and (stringp command) command)))))

;;;###autoload
(defun +tasks--warn-missing-just (recipe-p just-p)
  (when (and recipe-p (not just-p))
    (message "justfile has a test recipe but `just' is not on exec-path; using projectile's test command")))

;;;###autoload
(defun +tasks--project-test-command (root)
  (let ((recipe-p (+tasks--test-command (+tasks--justfile-text root)))
        (just-p (executable-find "just")))
    (+tasks--warn-missing-just recipe-p just-p)
    (+tasks--choose-test-command recipe-p just-p (+tasks--projectile-test-command root))))

;;;###autoload
(defun +tasks--test-argv (root command)
  "COMMAND as a shell call, in the agent jail when it applies: the tests are agent code."
  (let ((argv (list shell-file-name shell-command-switch command)))
    (if (+tasks--agent :jail) (+tasks--agent-call :jail root argv) argv)))

;;;###autoload
(defun +tasks--shell-line (argv)
  (mapconcat #'shell-quote-argument argv " "))

;;;###autoload
(defun +tasks--test-root (&optional task)
  "TASK's worktree, by default the task at point in tasks.org, else the current project."
  (or (when-let* ((task (or task (+tasks--linked-task-at-point))))
        (+tasks--task-worktree-of task))
      (+project-root)))

;;;###autoload
(defun +tasks--compile-tests (task root command)
  (with-current-buffer (compile (+tasks--shell-line (+tasks--test-argv root command)))
    (setq +tasks--compiled-task task)))

;;;###autoload
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

;;;###autoload
(defun +tasks--merge-message (branch heading)
  (format "Merge %s: %s" branch heading))

;;;###autoload
(defun +tasks--dirty-p ()
  (+git-lines "status" "--porcelain" "--untracked-files=no"))

;;;###autoload
(defun +tasks--switch (branch)
  (unless (+git-ok-p "switch" branch)
    (user-error "git could not switch to %s" branch)))

;;;###autoload
(defun +tasks--run-tests (root command)
  (let ((default-directory root)
        (buffer (get-buffer-create "*tasks-done-tests*"))
        (argv (+tasks--test-argv root command)))
    (with-current-buffer buffer (erase-buffer))
    (message "Running %s in %s..." command root)
    (unless (zerop (apply #'call-process (car argv) nil buffer nil (cdr argv)))
      (display-buffer buffer)
      (user-error "%s failed; nothing merged" command))))

;;;###autoload
(defun +tasks--run-tests-or-confirm (root)
  "Run ROOT's tests there, or ask to go on without any."
  (if-let* ((command (+tasks--project-test-command root)))
      (+tasks--run-tests root command)
    (unless (y-or-n-p "No test command found; merge without running tests? ")
      (user-error "Stopped before merging"))))

;;;###autoload
(defun +tasks--fetch (main)
  "Refresh origin/MAIN when there is an origin; what it held last time may be stale."
  (when (+git-ok-p "remote" "get-url" "origin")
    (+git-ok-p "fetch" "--quiet" "origin" main)))

;;;###autoload
(defun +tasks--catch-up (branch main)
  (+tasks--fetch main)
  (when (and (not (+git-ok-p "merge-base" "--is-ancestor" branch main))
             (+git-ok-p "merge-base" "--is-ancestor" branch (concat "origin/" main)))
    (unless (+git-ok-p "merge" "--ff-only" (concat "origin/" main))
      (user-error "%s is merged on origin, but %s cannot fast-forward to it" branch main))))

;;;###autoload
(defun +tasks--merge (branch main heading)
  (+tasks--switch main)
  (+tasks--catch-up branch main)
  (unless (+git-ok-p "merge-base" "--is-ancestor" branch main)
    (unless (+git-ok-p "merge" "--no-ff" "-m" (+tasks--merge-message branch heading) branch)
      (user-error "Merging %s stopped; finish it in magit, then run done again" branch))))

;;;###autoload
(defun +tasks--delete-local-branch (branch)
  "Delete BRANCH once HEAD contains it; plain -d also wants its upstream to."
  (unless (and (+git-ok-p "merge-base" "--is-ancestor" branch "HEAD")
               (+git-ok-p "branch" "-D" branch))
    (user-error "%s is not merged into HEAD; kept it" branch)))

;;;###autoload
(defun +tasks--delete-branch (branch)
  (+tasks--delete-local-branch branch)
  (when (and (+git-ok-p "show-ref" "--verify" "--quiet" (concat "refs/remotes/origin/" branch))
             (y-or-n-p (format "Also delete origin/%s? " branch)))
    (+git-ok-p "push" "origin" "--delete" branch)))

;;;###autoload
(defun +tasks--test-branch (root branch tree)
  "Test BRANCH in its worktree TREE, or checked out in ROOT when it has none."
  (if tree
      (+tasks--run-tests-or-confirm tree)
    (+tasks--switch branch)
    (+tasks--run-tests-or-confirm root)))

;;;###autoload
(defun +tasks--mark-done (branch buffer)
  (with-current-buffer buffer
    (goto-char (nth 2 (+tasks--branch-task branch)))
    (org-todo "DONE")
    (+tasks/refresh-branches)
    (save-buffer)))

;;;###autoload
(defmacro +tasks--or-set-state (task state &rest body)
  "Run BODY; should it stop with a user error, leave TASK in STATE, the error as the reason."
  (declare (indent 2))
  `(condition-case err
       (progn ,@body)
     (user-error (+tasks--set-state ,task ,state :reason (error-message-string err))
                 (signal (car err) (cdr err)))))

;;;###autoload
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

;;;###autoload
(defun +tasks--state-before-start ()
  "The state this task had before it was started, by its log; TODO when the log is silent."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t) (point)))
          (from nil))
      (while (re-search-forward +tasks--started-log-regexp end t)
        (setq from (match-string-no-properties 1)))
      (or from "TODO"))))

;;;###autoload
(defun +tasks--abort-entry ()
  "Unlink the task at point from its branch and return it to the state it was started from."
  (org-back-to-heading t)
  (let ((state (+tasks--state-before-start)))
    (org-entry-delete nil "BRANCH")
    (org-entry-delete nil "REASON")
    (org-todo state)))

;;;###autoload
(defun +tasks--stop-session (tree branch)
  (when tree
    (+tasks--agent-call :stop tree branch)))

;;;###autoload
(defun +tasks--backup-ref (branch)
  (concat "refs/aborted/" branch))

;;;###autoload
(defun +tasks--drop-branch (branch)
  "Delete BRANCH, keeping its tip under refs/aborted/ so the dropped work can still be read."
  (+git-ok-p "update-ref" (+tasks--backup-ref branch) branch)
  (unless (+git-ok-p "branch" "-D" branch)
    (user-error "git could not delete %s" branch)))

;;;###autoload
(defun +tasks--abort-question (task branch main)
  (format "Abort \"%s\": throw away %s (%d commit%s ahead of %s) and its worktree? "
          (plist-get task :heading) branch (+tasks--ahead branch main)
          (if (= 1 (+tasks--ahead branch main)) "" "s") main))

;;;###autoload
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