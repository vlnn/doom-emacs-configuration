;;; test/tasks-test.el -*- lexical-binding: t; -*-

(defmacro +tasks-test--with-projects (names &rest body)
  "Run BODY with `root' bound to a temp dir holding project dirs NAMES."
  (declare (indent 1))
  `(let ((root (make-temp-file "tasks-test" t)))
     (unwind-protect
         (progn
           (dolist (name ,names)
             (make-directory (expand-file-name name root)))
           ,@body)
       (delete-directory root t))))

(defun +tasks-test--touch (file)
  (with-temp-file file))

(ert-deftest +tasks--project-file/joins-root-and-name ()
  (ert-info ("project-file should place tasks.org at the project root")
    (should (equal "/repo/tasks.org" (+tasks--project-file "/repo/")))))

(ert-deftest +tasks--project-files/keeps-roots-with-tasks-org ()
  (+tasks-test--with-projects '("with" "without")
    (+tasks-test--touch (expand-file-name "with/tasks.org" root))
    (ert-info ("project-files should keep only roots that have a tasks.org")
      (should (equal (list (expand-file-name "with/tasks.org" root))
                     (+tasks--project-files
                      (list (expand-file-name "with/" root)
                            (expand-file-name "without/" root))))))))

(ert-deftest +tasks--project-files/skips-remote-roots ()
  (ert-info ("project-files should never touch remote projects")
    (should-not (+tasks--project-files '("/ssh:host:/srv/repo/")))))

(ert-deftest +tasks--agenda-files/extends-base ()
  (+tasks-test--with-projects '("a")
    (let ((tasks (expand-file-name "a/tasks.org" root)))
      (+tasks-test--touch tasks)
      (ert-info ("agenda-files should keep the base and add each project's tasks.org once")
        (should (equal (list "~/org/" tasks)
                       (+tasks--agenda-files
                        '("~/org/")
                        (list (expand-file-name "a/" root) (expand-file-name "a/" root)))))))))

(ert-deftest +tasks--branch-patterns/globs-each-prefix ()
  (pcase-dolist (`(,prefixes . ,expected) '((("feat/") . ("feat/*"))
                                            (("feat/" "fix/") . ("feat/*" "fix/*"))
                                            (() . ())))
    (ert-info ((format "branch-patterns of %S should be %S" prefixes expected))
      (should (equal expected (+tasks--branch-patterns prefixes))))))

(ert-deftest +tasks--parse-branch-line/splits-on-tab ()
  (ert-info ("parse-branch-line should give (branch . age)")
    (should (equal '("feat/x" . "2 days ago")
                   (+tasks--parse-branch-line "feat/x\t2 days ago")))))

(ert-deftest +tasks--task-for-branch/finds-heading-by-branch-property ()
  (with-temp-buffer
    (insert "* Active\n** TODO Tracked thing\n:PROPERTIES:\n:BRANCH: feat/tracked\n:END:\n")
    (org-mode)
    (pcase-dolist (`(,branch . ,expected) '(("feat/tracked" . "Tracked thing")
                                            ("feat/orphan" . nil)))
      (ert-info ((format "task-for-branch %s should be %S" branch expected))
        (should (equal expected (+tasks--task-for-branch branch)))))))

(ert-deftest +tasks--branch-row/marks-missing-task ()
  (pcase-dolist (`(,task . ,expected) '(("Add X" . "| feat/x | 2 days ago | Add X |\n")
                                        (nil . "| feat/x | 2 days ago | NONE |\n")))
    (ert-info ((format "branch-row with task %S should be %S" task expected))
      (should (equal expected (+tasks--branch-row "feat/x" "2 days ago" task))))))

(ert-deftest +tasks--skeleton/declares-flow-keywords-and-archive ()
  (let ((skeleton (+tasks--skeleton)))
    (pcase-dolist (`(,needle . ,why)
                   '(("#+TODO: IDEA(i) TODO(t) PLAN(p!) RED(r!) GREEN(g!) REVIEW(v!) | DONE(d!) DROPPED(x@)" . "declare the flow keywords")
                     ("#+ARCHIVE: %s_archive::* Archived" . "archive per file")
                     ("\n* Ideas\n" . "have the Ideas heading capture writes to")
                     ("#+BEGIN: branches\n#+END:" . "carry the branches block")))
      (ert-info ((format "skeleton should %s" why))
        (should (string-search needle skeleton))))))

(ert-deftest +tasks--string-list-p/accepts-only-string-lists ()
  (pcase-dolist (`(,value . ,expected) '((("feat/" "fix/") . t)
                                         (nil . t)
                                         ("feat/" . nil)
                                         ((feat) . nil)))
    (ert-info ((format "%S should%s be a safe prefix list" value (if expected "" " not")))
      (should (eq expected (+tasks--string-list-p value))))))
