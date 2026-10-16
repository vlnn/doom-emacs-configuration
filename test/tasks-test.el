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
  (pcase-dolist (`(,task . ,expected) '(("Add X" . "| feat/x | 2 days ago | Add X | ahead 2 |")
                                        (nil . "| feat/x | 2 days ago | NONE | ahead 2 |")))
    (ert-info ((format "branch-row with task %S should be %S" task expected))
      (should (equal expected (+tasks--branch-row "feat/x" "2 days ago" task "ahead 2"))))))

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

(defmacro +tasks-test--in-tasks-buffer (extra &rest body)
  "Run BODY in an org buffer holding the skeleton with EXTRA appended under * Ideas."
  (declare (indent 1))
  `(with-temp-buffer
     (insert (replace-regexp-in-string "^\\* Ideas\n" (concat "* Ideas\n" ,extra) (+tasks--skeleton)))
     (org-mode)
     (goto-char (point-min))
     ,@body))

(defun +tasks-test--goto-heading (title)
  (goto-char (point-min))
  (re-search-forward (concat "^\\*+ .*" (regexp-quote title))))

(ert-deftest +tasks--task-for-branch/ignores-structural-headings ()
  (with-temp-buffer
    (insert "#+TODO: IDEA TODO | DONE\n* Branches\n:PROPERTIES:\n:BRANCH: feat/n\n:END:\n")
    (org-mode)
    (ert-info ("task-for-branch should only match headings that carry a TODO keyword")
      (should-not (+tasks--task-for-branch "feat/n")))))

(ert-deftest +tasks--branch-table/no-trailing-newline ()
  (ert-info ("branch-table should end on the last row so the block has no blank line before #+END:")
    (should (equal "| Branch | Last commit | Task | State |\n|-\n| feat/x | now | NONE | merged |"
                   (+tasks--branch-table '("| feat/x | now | NONE | merged |"))))))

(defconst +tasks-test--aligned
  (concat "| Branch | Last commit | Task | State  |\n"
          "|--------+-------------+------+--------|\n"
          "| feat/x | now         | NONE | merged |"))

(defmacro +tasks-test--with-block (body-text &rest body)
  "Run BODY in an org buffer whose branches block holds BODY-TEXT, rows stubbed."
  (declare (indent 1))
  `(with-temp-buffer
     (insert "* Branches\n#+BEGIN: branches\n" ,body-text "\n#+END:\n")
     (org-mode)
     (set-buffer-modified-p nil)
     (cl-letf (((symbol-function '+tasks--branch-rows)
                (lambda (_) '("| feat/x | now | NONE | merged |"))))
       ,@body)))

(defun +tasks-test--block-body ()
  (goto-char (point-min))
  (re-search-forward "^#\\+BEGIN: branches\n")
  (buffer-substring (point) (progn (re-search-forward "\n#\\+END:") (match-beginning 0))))

(ert-deftest +tasks--aligned-table/aligns-outside-the-buffer ()
  (ert-info ("aligned-table should return the org-aligned table without a trailing newline")
    (should (equal +tasks-test--aligned
                   (+tasks--aligned-table '("| feat/x | now | NONE | merged |"))))))

(ert-deftest +tasks/refresh-branches/leaves-an-up-to-date-block-alone ()
  (+tasks-test--with-block +tasks-test--aligned
    (+tasks/refresh-branches)
    (ert-info ("refresh-branches should not touch the buffer when the table is unchanged")
      (should-not (buffer-modified-p)))))

(ert-deftest +tasks/refresh-branches/rewrites-a-stale-block ()
  (+tasks-test--with-block "| old | row |"
    (+tasks/refresh-branches)
    (ert-info ("refresh-branches should replace a stale block with the aligned table")
      (should (equal +tasks-test--aligned (+tasks-test--block-body))))
    (ert-info ("the rewritten rows should parse as table rows of the block")
      (goto-char (point-min))
      (re-search-forward "^| feat/x")
      (should (eq 'table-row (org-element-type (org-element-at-point)))))))

(ert-deftest +tasks--slug/lowercase-ascii-words-cut-at-word ()
  (pcase-dolist (`(,heading . ,expected)
                 '(("Radically simplify wisp flow unifying the menus over different scenarios"
                    . "radically-simplify-wisp-flow-unifying")
                   ("Run REPL inside LÖVE games!" . "run-repl-inside-love-games")
                   ("  Fix: off-by-one in #42  " . "fix-off-by-one-in-42")
                   ("Supercalifragilisticexpialidociousnessandthensomemore" . "supercalifragilisticexpialidociousnessan")))
    (ert-info ((format "slug of %S should be %S" heading expected))
      (should (equal expected (+tasks--slug heading))))))

(ert-deftest +tasks--branch-name/joins-prefix-and-slug ()
  (ert-info ("branch-name should prepend the prefix to the heading's slug")
    (should (equal "fix/broken-capture" (+tasks--branch-name "fix/" "Broken capture")))))

(ert-deftest +tasks--branch-title/turns-branch-back-into-words ()
  (pcase-dolist (`(,branch . ,expected) '(("feat/menu-unify" . "menu unify")
                                          ("fix/x" . "x")
                                          ("other" . "other")))
    (ert-info ((format "branch-title of %s should be %S" branch expected))
      (should (equal expected (+tasks--branch-title branch '("feat/" "fix/")))))))

(ert-deftest +tasks--row-branch/reads-data-rows-only ()
  (pcase-dolist (`(,line . ,expected) '(("| feat/x | 2 days ago | NONE |" . "feat/x")
                                        ("| Branch | Last commit | Task |" . nil)
                                        ("|--------+-------------+------|" . nil)
                                        ("plain text" . nil)))
    (ert-info ((format "row-branch of %S should be %S" line expected))
      (should (equal expected (+tasks--row-branch line))))))

(ert-deftest +tasks--tasks-file-p/matches-tasks-org-only ()
  (pcase-dolist (`(,file . ,expected) '(("/r/tasks.org" . t) ("/r/notes.org" . nil) (nil . nil)))
    (ert-info ((format "tasks-file-p %S should be %S" file expected))
      (should (eq expected (+tasks--tasks-file-p file))))))

(ert-deftest +tasks--open-tasks/lists-ideas-and-todos ()
  (+tasks-test--in-tasks-buffer "** IDEA First\n** DONE Old\n"
    (save-excursion (re-search-forward "^\\* Active\n") (insert "** TODO Second\n"))
    (ert-info ("open-tasks should list IDEA and TODO headings with their positions")
      (should (equal '("First" "Second") (mapcar #'car (+tasks--open-tasks)))))))

(ert-deftest +tasks--start-entry/promotes-links-and-refiles ()
  (+tasks-test--in-tasks-buffer "** IDEA Unify menus\n"
    (+tasks-test--goto-heading "Unify menus")
    (+tasks--start-entry "feat/unify-menus" "one entry point")
    (+tasks-test--goto-heading "Unify menus")
    (pcase-dolist (`(,actual ,expected ,why)
                   `((,(org-get-todo-state) "TODO" "switch IDEA to TODO")
                     (,(org-entry-get nil "BRANCH") "feat/unify-menus" "set the branch")
                     (,(org-entry-get nil "ACCEPT") "one entry point" "set the acceptance line")
                     (,(org-entry-get nil "MODEL") "local" "default the model to local")
                     (,(save-excursion (org-up-heading-safe) (org-get-heading t t t t)) "Active" "refile under Active")))
      (ert-info ((format "start-entry should %s" why))
        (should (equal expected actual))))))

(ert-deftest +tasks--start-entry/keeps-existing-model-and-accept ()
  (+tasks-test--in-tasks-buffer "** TODO Keep\n:PROPERTIES:\n:MODEL: claude\n:ACCEPT: old\n:END:\n"
    (+tasks-test--goto-heading "Keep")
    (+tasks--start-entry "feat/keep" nil)
    (+tasks-test--goto-heading "Keep")
    (ert-info ("start-entry should not overwrite a model or acceptance line already set")
      (should (equal '("claude" "old") (list (org-entry-get nil "MODEL") (org-entry-get nil "ACCEPT")))))))

(ert-deftest +tasks--insert-idea/adds-linked-idea-under-ideas ()
  (+tasks-test--in-tasks-buffer ""
    (+tasks--insert-idea "menu unify" "feat/menu-unify")
    (+tasks-test--goto-heading "menu unify")
    (ert-info ("insert-idea should create an IDEA carrying the branch under * Ideas")
      (should (equal '("IDEA" "feat/menu-unify" "Ideas")
                     (list (org-get-todo-state) (org-entry-get nil "BRANCH")
                           (save-excursion (org-up-heading-safe) (org-get-heading t t t t))))))))

(ert-deftest +tasks--feature-prompt/names-the-heading ()
  (ert-info ("feature-prompt should be the /feature command for the task")
    (should (equal "/feature Unify menus" (+tasks--feature-prompt "Unify menus")))))

(ert-deftest +tasks/menu-file/defines-the-transient ()
  (require 'transient)
  (load (expand-file-name "modules/my/tasks/menu.el" default-directory) nil t)
  (ert-info ("menu.el should define the +tasks-transient prefix")
    (should (commandp '+tasks-transient))))

(ert-deftest +tasks--agenda-files/one-entry-per-file-across-spellings ()
  (+tasks-test--with-projects '("real")
    (let ((real (expand-file-name "real/" root))
          (link (expand-file-name "link/" root)))
      (+tasks-test--touch (expand-file-name "tasks.org" real))
      (make-symbolic-link (directory-file-name real) (directory-file-name link))
      (ert-info ("agenda-files should list a tasks.org once even when projectile knows it by two paths")
        (should (equal 1 (length (+tasks--agenda-files nil (list real link
                                                                  (concat real "./"))))))))))

;;; review, test, done

(ert-deftest +tasks--state-cell/flags-merged-ahead-and-stale ()
  (pcase-dolist (`(,merged ,ahead ,task-state ,expected)
                 '((t 0 "REVIEW" "merged")
                   (nil 3 "GREEN" "ahead 3")
                   (nil 1 nil "ahead 1")
                   (t 0 "DONE" "DONE! delete branch")
                   (nil 2 "DONE" "DONE! ahead 2")))
    (ert-info ((format "state-cell merged=%s ahead=%s task=%s should be %S" merged ahead task-state expected))
      (should (equal expected (+tasks--state-cell merged ahead task-state))))))

(ert-deftest +tasks--default-branch-from/prefers-origin-head ()
  (pcase-dolist (`(,origin-head ,locals ,expected)
                 '(("refs/remotes/origin/develop" ("main" "develop") "develop")
                   (nil ("master") "master")
                   (nil ("main" "master") "main")
                   (nil () "main")))
    (ert-info ((format "default-branch-from %S %S should be %s" origin-head locals expected))
      (should (equal expected (+tasks--default-branch-from origin-head locals))))))

(ert-deftest +tasks--test-command/uses-a-just-test-recipe ()
  (pcase-dolist (`(,justfile . ,expected) '(("test:\n\tpytest\n" . "just test")
                                            ("check:\n\truff\n\ntest *args:\n\tpytest {{args}}\n" . "just test")
                                            ("testing:\n\techo\n" . nil)
                                            ("check:\n\truff\n" . nil)
                                            (nil . nil)))
    (ert-info ((format "test-command for justfile %S should be %S" justfile expected))
      (should (equal expected (+tasks--test-command justfile))))))

(ert-deftest +tasks--merge-message/names-branch-and-task ()
  (ert-info ("merge-message should name the branch and the task heading")
    (should (equal "Merge feat/x: Unify menus" (+tasks--merge-message "feat/x" "Unify menus")))))

(ert-deftest +tasks--review-prompt/carries-accept-and-diff ()
  (let ((prompt (+tasks--review-prompt "one entry point" "diff --git a/x b/x")))
    (pcase-dolist (`(,needle . ,why) '(("one entry point" . "state the acceptance criterion")
                                       ("diff --git a/x b/x" . "include the diff")))
      (ert-info ((format "review-prompt should %s" why))
        (should (string-search needle prompt))))))

(ert-deftest +tasks--exclude-lines/adds-only-missing-entries ()
  (pcase-dolist (`(,existing . ,expected) '(("" . ("tasks.org" "tasks.org_archive"))
                                            ("# git ls-files\ntasks.org\n" . ("tasks.org_archive"))
                                            ("tasks.org\ntasks.org_archive\n" . ())))
    (ert-info ((format "exclude-lines for %S should be %S" existing expected))
      (should (equal expected (+tasks--exclude-lines existing))))))

(ert-deftest +tasks--branch-task/returns-heading-state-and-position ()
  (+tasks-test--in-tasks-buffer "** DONE Shipped\n:PROPERTIES:\n:BRANCH: feat/shipped\n:END:\n"
    (pcase-let ((`(,heading ,state ,pos) (+tasks--branch-task "feat/shipped")))
      (ert-info ("branch-task should give the linked task's heading and state")
        (should (equal '("Shipped" "DONE") (list heading state))))
      (ert-info ("branch-task should give the linked task's position")
        (goto-char pos)
        (should (equal "Shipped" (org-get-heading t t t t)))))
    (ert-info ("branch-task should be nil for an unlinked branch")
      (should-not (+tasks--branch-task "feat/other")))))

(ert-deftest +tasks/menu-file/binds-the-work-keys ()
  (require 'transient)
  (load (expand-file-name "modules/my/tasks/menu.el" default-directory) nil t)
  (pcase-dolist (`(,key . ,command) '(("r" . +tasks/review) ("R" . +tasks/ai-review)
                                      ("t" . +tasks/test) ("d" . +tasks/done)))
    (ert-info ((format "menu key %s should run %s" key command))
      (should (eq command (plist-get (nth 2 (transient-get-suffix '+tasks-transient key)) :command))))))
