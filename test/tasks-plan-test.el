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
