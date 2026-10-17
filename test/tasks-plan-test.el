;;; test/tasks-plan-test.el -*- lexical-binding: t; -*-

(defconst +tasks-plan-test--written
  "# Plan: Fix it

ACCEPT: it works

## Decisions
- Q: where? → A: here

## Tests, in order
- [x] `a/one`: a should one
- [ ] `a/two`: a should two

## Functions
- `+a--one` in `a.el`: does one

## Touches guarded files
- none

## Open
")

(defconst +tasks-plan-test--approved
  (replace-regexp-in-string "^ACCEPT: it works\n" "ACCEPT: it works\nApproved: 2026-10-17\n" +tasks-plan-test--written))

(defmacro +tasks-plan-test--with-worktree (plan-text &rest body)
  "Run BODY with `tree' bound to a temp dir holding PLAN.md with PLAN-TEXT, or nothing when nil."
  (declare (indent 1))
  `(let ((tree (file-name-as-directory (file-truename (make-temp-file "tasks-plan-test" t)))))
     (unwind-protect
         (progn
           (when ,plan-text
             (with-temp-file (expand-file-name "PLAN.md" tree) (insert ,plan-text)))
           ,@body)
       (delete-directory tree t))))

(ert-deftest +tasks--excluded-files/keeps-the-plan-out-of-git ()
  (ert-info ("the plan should be excluded from git, since it lives in the worktree and is not part of the change")
    (should (member "PLAN.md" (+tasks--excluded-files)))))

(ert-deftest +tasks--plan-file/sits-at-the-worktree-root ()
  (ert-info ("plan-file should be PLAN.md at the root of the given worktree")
    (should (equal "/r/.worktrees/feat-x/PLAN.md" (+tasks--plan-file "/r/.worktrees/feat-x/"))))
  (ert-info ("plan-file should be nil for a task without a worktree")
    (should-not (+tasks--plan-file nil))))

(ert-deftest +tasks--plan-text/reads-the-plan-when-there-is-one ()
  (+tasks-plan-test--with-worktree +tasks-plan-test--written
    (ert-info ("plan-text should read PLAN.md from the worktree")
      (should (equal +tasks-plan-test--written (+tasks--plan-text tree)))))
  (+tasks-plan-test--with-worktree nil
    (ert-info ("plan-text should be nil while the interview has not written the file")
      (should-not (+tasks--plan-text tree))))
  (ert-info ("plan-text should be nil without a worktree")
    (should-not (+tasks--plan-text nil))))

(ert-deftest +tasks--plan-approved-p/reads-the-approved-line ()
  (pcase-dolist (`(,text ,expected ,why) `((,+tasks-plan-test--approved t "accept a plan carrying the Approved line")
                                           (,+tasks-plan-test--written nil "refuse a plan without it")
                                           ("# Plan\n\nApproved by nobody\n" nil "want the exact Approved: line, not the word")
                                           (nil nil "be nil without a plan")))
    (ert-info ((format "plan-approved-p should %s" why))
      (should (eq expected (+tasks--plan-approved-p text))))))

(ert-deftest +tasks--plan-mark/says-what-the-plan-waits-for ()
  (pcase-dolist (`(,text . ,expected) `((nil . "plan?")
                                        (,+tasks-plan-test--written . "approve!")
                                        (,+tasks-plan-test--approved . nil)))
    (ert-info ((format "plan-mark for %s should be %S" (if text "a written plan" "no plan") expected))
      (should (equal expected (+tasks--plan-mark text))))))

(ert-deftest +tasks--agenda-prefix/shows-plan-marks-instead-of-the-branch ()
  (+tasks-plan-test--with-worktree +tasks-plan-test--written
    (+tasks-test--in-tasks-buffer "** PLAN Written\n:PROPERTIES:\n:BRANCH: feat/written\n:END:\n** PLAN Unwritten\n:PROPERTIES:\n:BRANCH: feat/unwritten\n:END:\n** RED Working\n:PROPERTIES:\n:BRANCH: feat/working\n:END:\n"
      (cl-letf (((symbol-function '+tasks--branch-worktree-in)
                 (lambda (_root branch) (and (equal branch "feat/written") tree))))
        (pcase-dolist (`(,heading . ,expected) '(("Written" . "approve!") ("Unwritten" . "plan?") ("Working" . "feat/working")))
          (+tasks-test--goto-heading heading)
          (ert-info ((format "the board prefix of %s should read %S" heading expected))
            (should (equal expected (+tasks--agenda-prefix)))))))))

;;; approving a plan

(ert-deftest +tasks--next-step/plan-asks-for-approval-not-tests ()
  (ert-info ("next on a PLAN task should offer the plan for approval; there are no tests to run yet")
    (should (eq '+tasks/approve-plan (+tasks--next-step "PLAN"))))
  (ert-info ("next on a RED task should still run the tests")
    (should (eq '+tasks/test (+tasks--next-step "RED")))))

(ert-deftest +tasks--plan-missing-heading/names-the-first-absent-section ()
  (pcase-dolist (`(,text . ,expected)
                 `((,+tasks-plan-test--written . nil)
                   (,(replace-regexp-in-string "^## Open\n" "" +tasks-plan-test--written) . "## Open")
                   (,(replace-regexp-in-string "^## Tests, in order\n\\|^## Functions\n" "" +tasks-plan-test--written) . "## Tests, in order")
                   (,(replace-regexp-in-string "^ACCEPT: .*\n" "" +tasks-plan-test--written) . "ACCEPT:")
                   ("# Plan\n" . "ACCEPT:")))
    (ert-info ((format "plan-missing-heading should be %S" expected))
      (should (equal expected (+tasks--plan-missing-heading text))))))

(ert-deftest +tasks--plan-with-approval/dates-the-plan-under-accept ()
  (ert-info ("approving should add the Approved line right after the ACCEPT line, once")
    (should (equal +tasks-plan-test--approved
                   (+tasks--plan-with-approval +tasks-plan-test--written "2026-10-17")))))

(defmacro +tasks-plan-test--approving (plan-text answer &rest body)
  "Run +tasks/approve-plan on a PLAN task whose worktree holds PLAN-TEXT, answering ANSWER.
BODY sees `tree', `tasks' (the task buffer), `sent' (lines sent to pi), `prefilled' and `resumed'."
  (declare (indent 2))
  `(+tasks-plan-test--with-worktree ,plan-text
     (+tasks-test--in-tasks-buffer "** PLAN Fix it\n:PROPERTIES:\n:BRANCH: feat/fix-it\n:MODEL: local\n:END:\n"
       (+tasks-test--goto-heading "Fix it")
       (let ((task (+tasks--task-plist))
             (tasks (current-buffer))
             sent prefilled resumed)
         (cl-letf (((symbol-function '+tasks--work-task) (lambda () task))
                   ((symbol-function '+tasks--branch-worktree-in) (lambda (&rest _) tree))
                   ((symbol-function '+tasks--read-approval) (lambda () ,answer))
                   ((symbol-function '+tasks--show-plan) #'ignore)
                   ((symbol-function '+tasks/resume) (lambda () (setq resumed t)))
                   ((symbol-function '+ai--send-pilish-input) (lambda (line &optional _) (push line sent)))
                   ((symbol-function '+ai--prefill-pilish-input) (lambda (line &optional _) (setq prefilled line)))
                   ((symbol-function 'message) #'ignore))
           (+tasks/approve-plan)
           (with-current-buffer tasks
             ,@body))))))

(ert-deftest +tasks/approve-plan/resumes-when-the-plan-is-not-written-yet ()
  (+tasks-plan-test--approving nil ?y
    (ert-info ("approve-plan without PLAN.md should take you back to the running interview")
      (should resumed))
    (ert-info ("approve-plan without PLAN.md should neither approve nor send anything")
      (should (equal '("PLAN" nil) (+tasks-test--state-of (current-buffer) "Fix it")))
      (should-not sent))))

(ert-deftest +tasks/approve-plan/refuses-a-plan-without-the-fixed-headings ()
  (+tasks-plan-test--with-worktree (replace-regexp-in-string "^## Open\n" "" +tasks-plan-test--written)
    (+tasks-test--in-tasks-buffer "** PLAN Fix it\n:PROPERTIES:\n:BRANCH: feat/fix-it\n:END:\n"
      (+tasks-test--goto-heading "Fix it")
      (let ((task (+tasks--task-plist)))
        (cl-letf (((symbol-function '+tasks--work-task) (lambda () task))
                  ((symbol-function '+tasks--branch-worktree-in) (lambda (&rest _) tree))
                  ((symbol-function '+tasks--read-approval) (lambda () (error "Should not ask"))))
          (ert-info ("approve-plan should stop, naming the missing section, before asking anything")
            (should (string-match-p "## Open" (cadr (should-error (+tasks/approve-plan) :type 'user-error))))))))))

(ert-deftest +tasks/approve-plan/approves-into-red-and-sends-the-go-line ()
  (+tasks-plan-test--approving +tasks-plan-test--written ?y
    (ert-info ("y should write the Approved line into PLAN.md")
      (should (+tasks--plan-approved-p (+tasks--plan-text tree))))
    (ert-info ("y should move the task to RED")
      (should (equal '("RED" nil) (+tasks-test--state-of (current-buffer) "Fix it"))))
    (ert-info ("y should tell pi to start with the first test, naming the file that carries the approval")
      (should (= 1 (length sent)))
      (should (string-match-p "Plan approved: PLAN.md carries the Approved line" (car sent)))
      (should (string-match-p "grilling interview" (car sent))))))

(ert-deftest +tasks/approve-plan/prefills-a-refusal ()
  (+tasks-plan-test--approving +tasks-plan-test--written ?n
    (ert-info ("n should leave the task in PLAN and the plan unapproved")
      (should (equal '("PLAN" nil) (+tasks-test--state-of (current-buffer) "Fix it")))
      (should-not (+tasks--plan-approved-p (+tasks--plan-text tree))))
    (ert-info ("n should prefill the refusal for you to finish, sending nothing")
      (should (equal "Plan not approved: " prefilled))
      (should-not sent))))

(ert-deftest +tasks/approve-plan/sends-the-reread-line-after-an-edit ()
  (+tasks-plan-test--approving +tasks-plan-test--written ?e
    (let ((plan (get-file-buffer (+tasks--plan-file tree))))
      (unwind-protect
          (progn
            (ert-info ("e should open PLAN.md for editing")
              (should plan)
              (should-not (buffer-local-value 'buffer-read-only plan)))
            (with-current-buffer plan
              (goto-char (point-max))
              (insert "- a change\n")
              (save-buffer))
            (ert-info ("saving the edited plan should ask pi to read it again and wait")
              (should (equal 1 (length sent)))
              (should (string-match-p "PLAN.md edited; read it again" (car sent))))
            (ert-info ("e should leave the task in PLAN, unapproved")
              (should (equal '("PLAN" nil) (+tasks-test--state-of tasks "Fix it")))))
        (when plan (kill-buffer plan))))))

(ert-deftest +tasks/unpark/returns-to-red-when-the-plan-is-approved ()
  (pcase-dolist (`(,plan-text . ,expected) `((,+tasks-plan-test--approved . "RED")
                                             (,+tasks-plan-test--written . "PLAN")
                                             (nil . "PLAN")))
    (+tasks-plan-test--with-worktree plan-text
      (+tasks-test--in-tasks-buffer "** WAIT Thing\n:PROPERTIES:\n:BRANCH: feat/thing\n:REASON: API key\n:END:\n"
        (+tasks-test--goto-heading "Thing")
        (cl-letf (((symbol-function '+tasks--work-task) (let ((task (+tasks--task-plist))) (lambda () task)))
                  ((symbol-function '+tasks--branch-worktree-in) (lambda (&rest _) tree))
                  ((symbol-function '+tasks/resume) #'ignore))
          (+tasks/unpark))
        (ert-info ((format "unpark should return to %s when the plan is %s" expected
                           (cond ((null plan-text) "not written") ((equal expected "RED") "approved") (t "unapproved"))))
          (should (equal (list expected nil) (+tasks-test--state-of (current-buffer) "Thing"))))))))

;;; review and done read the plan

(ert-deftest +tasks--plan-tick-count/counts-checked-tests ()
  (pcase-dolist (`(,text . ,expected) `((,+tasks-plan-test--written . (1 . 2))
                                        (,(concat +tasks-plan-test--written "- [ ] an open item\n") . (1 . 2))
                                        ("# Plan\n\n## Tests, in order\n\n## Open\n" . (0 . 0))))
    (ert-info ((format "tick-count should be %S, counting only the boxes under Tests" expected))
      (should (equal expected (+tasks--plan-tick-count text))))))

(ert-deftest +tasks--plan-open-items/lists-the-bullets-under-open ()
  (ert-info ("open-items should be empty for a plan whose Open section has no bullet")
    (should-not (+tasks--plan-open-items +tasks-plan-test--written)))
  (ert-info ("open-items should list each bullet under Open, and nothing from other sections")
    (should (equal '("- which backend?" "- naming")
                   (+tasks--plan-open-items (concat +tasks-plan-test--written "- which backend?\n- naming\n")))))
  (ert-info ("open-items should be nil without a plan")
    (should-not (+tasks--plan-open-items nil))))

(ert-deftest +tasks--review-prompt/carries-the-plan-when-there-is-one ()
  (let ((with-plan (+tasks--review-prompt "it works" "diff --git a/x b/x" +tasks-plan-test--written))
        (without (+tasks--review-prompt "it works" "diff --git a/x b/x" nil)))
    (ert-info ("review-prompt should put the plan the agent committed to before the diff")
      (should (< (string-search "Plan the agent committed to:" with-plan)
                 (string-search "## Decisions" with-plan)
                 (string-search "diff --git" with-plan))))
    (ert-info ("review-prompt with a plan should ask about departures from it")
      (should (string-search "departs from the plan" with-plan)))
    (ert-info ("review-prompt without a plan should read as before")
      (should-not (string-search "Plan the agent committed to:" without))
      (should (string-search "diff --git a/x b/x" without)))))

(ert-deftest +tasks--review-message/adds-the-tick-count-after-the-guard-note ()
  (ert-info ("review-message should end with the tick count when there is a plan")
    (should (equal "ACCEPT: it works\nTouches agent guards: AGENTS.md\nplan: 1/2 tests ticked"
                   (+tasks--review-message "it works" "Touches agent guards: AGENTS.md" +tasks-plan-test--written))))
  (ert-info ("review-message should say nothing about a plan when there is none")
    (should (equal "ACCEPT: it works" (+tasks--review-message "it works" nil nil)))))

(defmacro +tasks-plan-test--done-with-plan (plan-text force &rest body)
  "Run (+tasks/done FORCE) on feat/x, whose worktree holds PLAN-TEXT; BODY sees `repo', `tree', `buffer'."
  (declare (indent 2))
  `(+tasks-test--in-git-repo
    (let* ((repo default-directory)
           (tree (+tasks--add-worktree repo "feat/x"))
           (buffer (find-file-noselect (expand-file-name "tasks.org" repo))))
      (unwind-protect
          (progn
            (let ((default-directory tree))
              (+tasks-test--commit-file "feature.txt" "done"))
            (with-temp-file (expand-file-name "PLAN.md" tree) (insert ,plan-text))
            (+tasks-test--write-active-task buffer "Ship it" "feat/x")
            (cl-letf (((symbol-function '+tasks--work-task) (lambda () (+tasks-test--task-in buffer "Ship it")))
                      ((symbol-function '+tasks--project-test-command) (lambda (_) "true"))
                      ((symbol-function 'message) #'ignore))
              ,@body))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest +tasks/done/refuses-while-the-plan-has-open-items ()
  (+tasks-plan-test--done-with-plan (concat +tasks-plan-test--written "- which backend?\n") nil
    (ert-info ("done should stop, listing the open items, while the plan's Open section has a bullet")
      (should (string-search "which backend?" (cadr (should-error (+tasks/done) :type 'user-error)))))
    (ert-info ("a refused done should merge nothing")
      (should-not (file-exists-p (expand-file-name "feature.txt" repo))))))

(ert-deftest +tasks/done/merges-with-a-prefix-arg-despite-open-items ()
  (+tasks-plan-test--done-with-plan (concat +tasks-plan-test--written "- which backend?\n") t
    (+tasks/done t)
    (ert-info ("done with a prefix argument should merge even with open items")
      (should (file-exists-p (expand-file-name "feature.txt" repo))))))

(ert-deftest +tasks/done/merges-a-planned-branch-with-nothing-open ()
  (+tasks-plan-test--done-with-plan +tasks-plan-test--approved nil
    (+tasks/done)
    (ert-info ("done should merge a planned branch whose Open section is bare, and take PLAN.md away with the worktree")
      (should (file-exists-p (expand-file-name "feature.txt" repo)))
      (should-not (file-exists-p tree)))))
