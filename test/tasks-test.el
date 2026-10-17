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

;;; top headings carrying tags, cookies or a priority

(defun +tasks-test--backlog-pos ()
  (save-excursion (goto-char (point-min)) (re-search-forward "^\\* Backlog") (line-beginning-position)))

(ert-deftest +tasks--top-heading-end/finds-headings-with-tags-cookies-or-priority ()
  (dolist (heading '("* Active" "* Active :work:" "* Active [1/3]" "* [#A] Active" "* Active [50%] :work:x:"))
    (with-temp-buffer
      (insert "* Ideas\n" heading "\n** TODO x\n* Backlog\n")
      (org-mode)
      (ert-info ((format "top-heading-end should find %S as the Active heading and end before * Backlog" heading))
        (should (= (+tasks-test--backlog-pos) (+tasks--top-heading-end "Active")))))))

(ert-deftest +tasks--top-heading-end/missing-heading-is-a-user-error ()
  (with-temp-buffer
    (insert "* Ideas\n* Activity\n* Backlog\n")
    (org-mode)
    (ert-info ("top-heading-end should name the missing heading in a user-error, not a failed search")
      (should-error (+tasks--top-heading-end "Active") :type 'user-error))))

(ert-deftest +tasks--move-under/keeps-subtree-when-target-missing ()
  (with-temp-buffer
    (insert "* Ideas\n** IDEA keep me\n")
    (org-mode)
    (+tasks-test--goto-heading "keep me")
    (beginning-of-line)
    (ert-info ("move-under should refuse when the target heading is missing")
      (should-error (+tasks--move-under "Active") :type 'user-error))
    (ert-info ("move-under should not delete the subtree when the target heading is missing")
      (should (string-match-p "IDEA keep me" (buffer-string))))))

(ert-deftest +tasks--start-entry/refiles-under-a-tagged-active ()
  (+tasks-test--in-tasks-buffer "** IDEA Keep me\n"
    (save-excursion
      (re-search-forward "^\\* Active$")
      (insert " :work:"))
    (+tasks-test--goto-heading "Keep me")
    (+tasks--start-entry "feat/keep-me" "it stays")
    (ert-info ("start-entry should refile the task under a * Active heading that carries tags")
      (should (string-match-p "^\\* Active :work:\n\\*\\* TODO Keep me" (buffer-string))))))

(ert-deftest +tasks--start-entry/leaves-active-tasks-in-place-under-a-cookie ()
  (+tasks-test--in-tasks-buffer ""
    (re-search-forward "^\\* Active$")
    (insert " [0/2]\n** TODO First\n** TODO Second")
    (+tasks-test--goto-heading "First")
    (+tasks--start-entry "feat/first" "it works")
    (ert-info ("start-entry should not reorder a task that already sits under * Active [0/2]")
      (should (string-match-p "TODO First\\(.\\|\n\\)*TODO Second" (buffer-string))))))

(ert-deftest +tasks--insert-idea/adds-linked-idea-under-ideas ()
  (+tasks-test--in-tasks-buffer ""
    (+tasks--insert-idea "menu unify" "feat/menu-unify")
    (+tasks-test--goto-heading "menu unify")
    (ert-info ("insert-idea should create an IDEA carrying the branch under * Ideas")
      (should (equal '("IDEA" "feat/menu-unify" "Ideas")
                     (list (org-get-todo-state) (org-entry-get nil "BRANCH")
                           (save-excursion (org-up-heading-safe) (org-get-heading t t t t))))))))

(ert-deftest +tasks--feature-prompt/names-the-heading-and-acceptance ()
  (pcase-dolist (`(,accept ,expected ,why)
                 '((nil "/feature Unify menus" "be the bare /feature command without ACCEPT")
                   ("one entry point" "/feature Unify menus\n\nACCEPT: one entry point" "carry the ACCEPT line")))
    (ert-info ((format "feature-prompt should %s" why))
      (should (equal expected (+tasks--feature-prompt "Unify menus" accept))))))

(ert-deftest +tasks--task-plist/carries-the-model ()
  (+tasks-test--in-tasks-buffer "** TODO Modelled\n:PROPERTIES:\n:BRANCH: feat/m\n:MODEL: claude\n:END:\n"
    (+tasks-test--goto-heading "Modelled")
    (ert-info ("task-plist should carry :MODEL: so resume opens the session on it")
      (should (equal "claude" (plist-get (+tasks--task-plist) :model))))))

(ert-deftest +tasks/resume/opens-the-task-worktree-and-focuses-its-session ()
  (let (launched added)
    (cl-letf (((symbol-function '+tasks--work-task)
               (lambda () (list :buffer (current-buffer) :heading "X" :branch "feat/x" :model "local")))
              ((symbol-function '+tasks--branch-worktree-in) (lambda (&rest _) nil))
              ((symbol-function '+tasks--add-worktree) (lambda (root branch) (setq added (list root branch))))
              ((symbol-function '+tasks--hand-off)
               (lambda (root &rest args) (setq launched (cons root args)) (funcall (plist-get args :prepare)))))
      (with-temp-buffer
        (setq default-directory "/r/")
        (+tasks/resume)))
    (ert-info ("resume should make sure the task's worktree exists once the review allows it")
      (should (equal '("/r/" "feat/x") added)))
    (ert-info ("resume should open the session named after the branch, in its worktree, on the task's model")
      (should (equal '("/r/.worktrees/feat-x/" "feat/x" "local" nil)
                     (list (car launched) (plist-get (cdr launched) :session)
                           (plist-get (cdr launched) :model) (plist-get (cdr launched) :draft)))))
    (ert-info ("resume should review the repo, not the worktree")
      (should (equal "/r/" (plist-get (cdr launched) :review-root))))))

(ert-deftest +tasks--open-tasks/in-flight-states ()
  (+tasks-test--in-tasks-buffer "** IDEA Idle\n"
    (save-excursion (re-search-forward "^\\* Active\n")
                    (insert "** RED Failing\n** GREEN Passing\n** DONE Shipped\n"))
    (ert-info ("open-tasks with the in-flight states should skip ideas and finished work")
      (should (equal '("Failing" "Passing") (mapcar #'car (+tasks--open-tasks +tasks-in-flight-states)))))))

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
                                            ("@test:\n\tpytest\n" . "just test")
                                            ("testing:\n\techo\n" . nil)
                                            ("test := \"x\"\n" . nil)
                                            ("check:\n\truff\n" . nil)
                                            (nil . nil)))
    (dolist (mode '(fundamental-mode emacs-lisp-mode org-mode))
      (with-temp-buffer
        (funcall mode)
        (ert-info ((format "test-command for justfile %S in %s should be %S" justfile mode expected))
          (should (equal expected (+tasks--test-command justfile))))))))

(ert-deftest +tasks--choose-test-command/needs-just-on-path-for-the-recipe ()
  (pcase-dolist (`(,recipe ,just ,projectile ,expected)
                 '((t   t   "pytest" "just test")
                   (t   nil "pytest" "pytest")
                   (nil t   "pytest" "pytest")
                   (t   nil nil      nil)
                   (nil nil nil      nil)))
    (ert-info ((format "recipe %s, just %s, projectile %S should run %S" recipe just projectile expected))
      (should (equal expected (+tasks--choose-test-command recipe just projectile))))))

(ert-deftest +tasks--projectile-test-command/keeps-only-shell-strings ()
  (pcase-dolist (`(,value . ,expected) '(("pytest" . "pytest") (ignore . nil) (nil . nil)))
    (cl-letf (((symbol-function 'projectile-test-command) (lambda (_) value)))
      (ert-info ((format "projectile test command %S should give %S" value expected))
        (should (equal expected (+tasks--projectile-test-command "/repo/")))))))

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
  (pcase-dolist (`(,existing . ,expected) '(("" . ("tasks.org" "tasks.org_archive" ".worktrees/"))
                                            ("# git ls-files\ntasks.org\n" . ("tasks.org_archive" ".worktrees/"))
                                            ("tasks.org\ntasks.org_archive\n.worktrees/\n" . ())))
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

(ert-deftest +tasks--proposed-branch/reuses-linked-branch ()
  (pcase-dolist (`(,extra ,expected ,why)
                 '(("** TODO Linked\n:PROPERTIES:\n:BRANCH: feat/kept\n:END:\n" "feat/kept" "reuse the branch already linked")
                   ("** TODO Fresh start\n" "feat/fresh-start" "derive a branch from the heading")))
    (+tasks-test--in-tasks-buffer extra
      (re-search-forward "^\\*\\* TODO ")
      (let ((+tasks-branch-prefixes '("feat/")))
        (ert-info ((format "proposed-branch should %s" why))
          (should (equal expected (+tasks--proposed-branch (org-get-heading t t t t)))))))))

(ert-deftest +tasks--project-category/names-the-repo ()
  (ert-info ("project-category should be the directory holding tasks.org")
    (should (equal "my-repo" (+tasks--project-category "/src/my-repo/tasks.org")))))

(ert-deftest +tasks--enable-file-mode-h/sets-agenda-category ()
  (with-temp-buffer
    (setq buffer-file-name "/src/my-repo/tasks.org")
    (+tasks--enable-file-mode-h)
    (ert-info ("a project's tasks.org should show in the agenda under the repo name, not \"tasks\"")
      (should (equal "my-repo" org-category)))
    (setq buffer-file-name nil)))

(ert-deftest +tasks--hand-off/leaves-side-effects-to-the-gate ()
  (let (launched prepared)
    (cl-letf (((symbol-function '+ai--launch-pi)
               (lambda (root &rest args) (setq launched (cons root args)))))
      (+tasks--hand-off "/r/" :draft "/feature X" :session "feat/x" :model "local"
                        :prepare (lambda () (setq prepared t))))
    (ert-info ("hand-off should pass root, draft, session and model to the launcher")
      (should (equal '("/r/" "/feature X" "feat/x" "local")
                     (list (car launched) (plist-get (cdr launched) :draft)
                           (plist-get (cdr launched) :session) (plist-get (cdr launched) :model)))))
    (ert-info ("hand-off should not run side effects itself; the launcher runs them after the review")
      (should-not prepared))))

(ert-deftest +tasks/menu-file/binds-continue ()
  (require 'transient)
  (load (expand-file-name "modules/my/tasks/menu.el" default-directory) nil t)
  (ert-info ("menu key c should resume the task's branch and session")
    (should (eq '+tasks/resume (plist-get (nth 2 (transient-get-suffix '+tasks-transient "c")) :command)))))

;;; idea trees

(defconst +tasks-test--tree
  "** IDEA Standalone\n** IDEA Parent\n*** IDEA Middle\n**** IDEA Deep\n*** IDEA Shallow\n*** DONE Finished\n")

(ert-deftest +tasks--has-open-descendant-p/sees-open-children-only ()
  (+tasks-test--in-tasks-buffer +tasks-test--tree
    (pcase-dolist (`(,title . ,expected) '(("Parent" . t) ("Middle" . t) ("Deep" . nil)
                                           ("Shallow" . nil) ("Standalone" . nil)))
      (+tasks-test--goto-heading title)
      (ert-info ((format "%s should%s have an open descendant" title (if expected "" " not")))
        (should (eq expected (and (+tasks--has-open-descendant-p) t)))))))

(ert-deftest +tasks--task-path/joins-task-ancestors ()
  (+tasks-test--in-tasks-buffer +tasks-test--tree
    (+tasks-test--goto-heading "Deep")
    (ert-info ("task-path should list task ancestors, stopping at the structural heading")
      (should (equal "Parent / Middle / Deep" (+tasks--task-path))))))

(ert-deftest +tasks--startable-tasks/leaves-deepest-first-per-tree ()
  (+tasks-test--in-tasks-buffer +tasks-test--tree
    (ert-info ("startable-tasks should keep file order of trees and put deeper leaves first within a tree")
      (should (equal '("Standalone" "Parent / Middle / Deep" "Parent / Shallow")
                     (mapcar #'car (+tasks--startable-tasks)))))))

(ert-deftest +tasks--startable-tasks/limits-to-subtree ()
  (+tasks-test--in-tasks-buffer +tasks-test--tree
    (+tasks-test--goto-heading "Middle")
    (ert-info ("startable-tasks within the tree should only offer leaves under point, with full paths")
      (should (equal '("Parent / Middle / Deep")
                     (mapcar #'car (+tasks--startable-tasks t)))))))

(ert-deftest +tasks--start-entry/refiles-whole-tree-keeping-shape ()
  (+tasks-test--in-tasks-buffer +tasks-test--tree
    (+tasks-test--goto-heading "Deep")
    (+tasks--start-entry "feat/deep" "deep works")
    (+tasks-test--goto-heading "Deep")
    (pcase-dolist (`(,actual ,expected ,why)
                   `((,(org-get-todo-state) "TODO" "promote only the started leaf")
                     (,(org-entry-get nil "BRANCH") "feat/deep" "link the leaf to its branch")
                     (,(+tasks--task-path) "Parent / Middle / Deep" "keep the leaf inside its tree")
                     (,(progn (+tasks-test--goto-heading "Parent")
                              (save-excursion (org-up-heading-safe) (org-get-heading t t t t)))
                      "Active" "move the tree root under Active")
                     (,(org-get-todo-state) "IDEA" "leave ancestors as ideas")))
      (ert-info ((format "start-entry on a nested idea should %s" why))
        (should (equal expected actual))))))

(ert-deftest +tasks--insert-child-idea/appends-last-child ()
  (+tasks-test--in-tasks-buffer +tasks-test--tree
    (+tasks-test--goto-heading "Parent")
    (+tasks--insert-child-idea "Later")
    (+tasks-test--goto-heading "Later")
    (ert-info ("insert-child-idea should add an IDEA one level below the task, after its children")
      (should (equal '("IDEA" 3 "Parent" "Finished")
                     (list (org-get-todo-state) (org-current-level)
                           (save-excursion (org-up-heading-safe) (org-get-heading t t t t))
                           (save-excursion (org-backward-heading-same-level 1) (org-get-heading t t t t))))))))

(ert-deftest +tasks-file-mode/blocks-closing-parents-with-open-children ()
  (pcase-dolist (`(,child . ,expected) '(("IDEA" . t) ("DONE" . nil)))
    (+tasks-test--in-tasks-buffer (format "** TODO Parent\n*** %s Child\n" child)
      (+tasks-file-mode 1)
      (+tasks-test--goto-heading "Parent")
      (ert-info ((format "a parent with a %s child should%s be blocked" child (if expected "" " not")))
        (should (eq expected (and (org-entry-blocked-p) t)))))))

(ert-deftest +tasks--skip-unless-leaf/skips-to-first-child ()
  (+tasks-test--in-tasks-buffer +tasks-test--tree
    (pcase-dolist (`(,title . ,expected) '(("Parent" . "Middle") ("Deep" . nil)))
      (+tasks-test--goto-heading title)
      (beginning-of-line)
      (ert-info ((format "agenda skip at %s should %s" title (if expected (concat "resume at " expected) "keep it")))
        (should (equal expected
                       (when-let* ((pos (+tasks--skip-unless-leaf)))
                         (save-excursion (goto-char pos) (org-get-heading t t t t)))))))))

(ert-deftest +tasks--in-order/keeps-candidate-order ()
  (let ((table (+tasks--in-order '("b" "a"))))
    (ert-info ("in-order should tell completion UIs not to re-sort candidates")
      (should (eq #'identity (completion-metadata-get (completion-metadata "" table nil)
                                                      'display-sort-function))))
    (ert-info ("in-order should still complete like a plain list")
      (should (equal '("b" "a") (all-completions "" table))))))

(ert-deftest +tasks--feature-prompt/names-parents-of-nested-tasks ()
  (pcase-dolist (`(,parents ,expected ,why)
                 '(("Parent / Middle" "/feature Deep\n\nPART OF: Parent / Middle\n\nACCEPT: works" "name the parents before ACCEPT")
                   (nil "/feature Deep\n\nACCEPT: works" "say nothing about parents of a top-level task")))
    (ert-info ((format "feature-prompt should %s" why))
      (should (equal expected (+tasks--feature-prompt "Deep" "works" parents))))))

(ert-deftest +tasks--task-parents/joins-task-ancestors-only ()
  (+tasks-test--in-tasks-buffer +tasks-test--tree
    (pcase-dolist (`(,title . ,expected) '(("Deep" . "Parent / Middle") ("Standalone" . nil)))
      (+tasks-test--goto-heading title)
      (ert-info ((format "task-parents of %s should be %S" title expected))
        (should (equal expected (+tasks--task-parents)))))))

;;; guard on diffs that widen what agents may do

(defmacro +tasks-test--in-git-repo (&rest body)
  "Run BODY with `default-directory' in a fresh repo holding one commit on main."
  `(let ((default-directory (file-name-as-directory (file-truename (make-temp-file "tasks-git-test" t)))))
     (unwind-protect
         (progn
           (+tasks-test--git "init" "-q" "-b" "main")
           (+tasks-test--git "config" "user.email" "test@example.com")
           (+tasks-test--git "config" "user.name" "test")
           (+tasks-test--commit-file "README.md" "readme")
           ,@body)
       (delete-directory default-directory t))))

(defun +tasks-test--git (&rest args)
  (apply #'call-process "git" nil nil nil
         "-c" "user.email=test@example.com" "-c" "user.name=test" args))

(defun +tasks-test--commit-file (relative content)
  (make-directory (file-name-directory (expand-file-name relative)) t)
  (with-temp-file relative (insert content))
  (+tasks-test--git "add" relative)
  (+tasks-test--git "commit" "-q" "-m" (concat "change " relative)))

(ert-deftest +tasks--guarded-p/flags-files-that-shape-agents ()
  (pcase-dolist (`(,file . ,expected)
                 '(("modules/my/ai/trust.el" . t)
                   ("modules/my/ai/agent.sb" . t)
                   ("modules/my/tasks/config.el" . t)
                   (".pi/extensions/x.ts" . t)
                   ("AGENTS.md" . t)
                   ("lib/AGENTS.md" . t)
                   (".dir-locals.el" . t)
                   ("sub/.dir-locals-2.el" . t)
                   ("justfile" . t)
                   (".justfile" . t)
                   ("README.md" . nil)
                   ("modules/my/vc/config.el" . nil)
                   ("test/tasks-test.el" . nil)
                   ("docs/AGENTS.md.bak" . nil)))
    (ert-info ((format "%s should%s count as an agent guard" file (if expected "" " not")))
      (should (eq expected (+tasks--guarded-p file))))))

(ert-deftest +tasks--guarded-p/flags-every-agent-context-file ()
  (dolist (file '(".agents/skills/x/SKILL.md" "CLAUDE.md" "sub/claude.md" "AGENTS.override.md"
                  "agents.MD" "Justfile" ".DIR-LOCALS.el"))
    (ert-info ((format "guard-p should flag %s, which agents or Emacs load with your rights" file))
      (should (+tasks--guarded-p file)))))

(ert-deftest +tasks--guarded-p/covers-what-the-trust-review-scans ()
  (dolist (name +ai-trust-context-files)
    (dolist (spelling (list name (downcase name) (upcase name)))
      (ert-info ((format "guard-p should flag %s, a context file the trust review scans" spelling))
        (should (+tasks--guarded-p spelling))))))

(ert-deftest +tasks--guard-note/lists-guarded-files-only ()
  (ert-info ("guard-note should name the guarded files among the changed ones")
    (should (equal "Touches agent guards: AGENTS.md, modules/my/ai/agent.sb"
                   (+tasks--guard-note '("README.md" "AGENTS.md" "modules/my/ai/agent.sb")))))
  (ert-info ("guard-note should be nil when no guarded file changed")
    (should-not (+tasks--guard-note '("README.md")))))

(ert-deftest +tasks--changed-files/lists-branch-changes-since-main ()
  (+tasks-test--in-git-repo
   (+tasks-test--git "switch" "-q" "-c" "feat/x")
   (+tasks-test--commit-file "modules/my/ai/agent.sb" "(version 1)")
   (+tasks-test--git "switch" "-q" "main")
   (+tasks-test--commit-file "later-on-main.txt" "x")
   (ert-info ("changed-files should list what the branch changed, not what main gained since")
     (should (equal '("modules/my/ai/agent.sb") (+tasks--changed-files "main" "feat/x"))))))

(ert-deftest +tasks--confirm-guarded/asks-only-when-guards-change ()
  (let (asked)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (prompt) (setq asked prompt) nil)))
      (+tasks--confirm-guarded "feat/x" '("README.md"))
      (ert-info ("confirm-guarded should not ask when no guarded file changed")
        (should-not asked))
      (ert-info ("confirm-guarded should stop the merge when the answer is no")
        (should-error (+tasks--confirm-guarded "feat/x" '("modules/my/ai/trust.el")) :type 'user-error))
      (ert-info ("confirm-guarded should name the branch and the guarded files in its question")
        (should (string-match-p "feat/x.*modules/my/ai/trust\\.el" asked))))))

(ert-deftest +tasks--confirm-guarded/passes-on-yes ()
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
    (ert-info ("confirm-guarded should let the merge go on after a yes")
      (should-not (condition-case nil
                      (progn (+tasks--confirm-guarded "feat/x" '("AGENTS.md")) nil)
                    (user-error t))))))

(ert-deftest +tasks--review-message/adds-the-guard-note-under-accept ()
  (ert-info ("review-message should show ACCEPT, then the guard note on its own line")
    (should (equal "ACCEPT: it works\nTouches agent guards: AGENTS.md"
                   (+tasks--review-message "it works" "Touches agent guards: AGENTS.md"))))
  (ert-info ("review-message should show only ACCEPT when no guard changed")
    (should (equal "ACCEPT: (none set)" (+tasks--review-message nil nil)))))

(ert-deftest +tasks--review-header/puts-the-guard-note-first ()
  (ert-info ("review-header should lead with the guard note so it is read before the model's review")
    (should (string-prefix-p "Touches agent guards: x\n\nReviewing feat/x on m"
                             (+tasks--review-header "feat/x" "m" "Touches agent guards: x"))))
  (ert-info ("review-header should start with the review line when no guard changed")
    (should (string-prefix-p "Reviewing feat/x" (+tasks--review-header "feat/x" "m" nil)))))

;;; tests run in the agent jail

(defmacro +tasks-test--jail-on (&rest body)
  `(let ((+ai-jail-enabled t)
         (+ai-jail-profile "/p/agent.sb"))
     (cl-letf (((symbol-function '+ai--jail-available-p) (lambda () t)))
       ,@body)))

(ert-deftest +tasks--test-argv/runs-the-command-through-the-shell-in-the-jail ()
  (+tasks-test--jail-on
   (let ((argv (+tasks--test-argv temporary-file-directory "bin/test")))
     (ert-info ("test-argv should start the jail first")
       (should (equal "sandbox-exec" (car argv))))
     (ert-info ("test-argv should run the test command through the shell, last")
       (should (equal (list shell-file-name shell-command-switch "bin/test") (last argv 3)))))))

(ert-deftest +tasks--test-argv/plain-shell-without-the-jail ()
  (cl-letf (((symbol-function '+ai--jailed) (lambda (_root command &rest _) command)))
    (ert-info ("test-argv should be a plain shell call when the jail does not apply")
      (should (equal (list shell-file-name shell-command-switch "just test")
                     (+tasks--test-argv temporary-file-directory "just test"))))))

(ert-deftest +tasks--run-tests-or-confirm/runs-tests-jailed ()
  (let (ran)
    (+tasks-test--jail-on
     (cl-letf (((symbol-function '+tasks--project-test-command) (lambda (_) "bin/test"))
               ((symbol-function 'message) #'ignore)
               ((symbol-function 'call-process)
                (lambda (program _infile _buffer _display &rest args) (setq ran (cons program args)) 0)))
       (+tasks--run-tests-or-confirm temporary-file-directory)))
    (ert-info ("done should run the branch's tests inside the jail, since they are the agent's code")
      (should (equal "sandbox-exec" (car ran)))
      (should (equal "bin/test" (car (last ran)))))))

(ert-deftest +tasks/test/compiles-the-jailed-command ()
  (let (compiled)
    (+tasks-test--jail-on
     (cl-letf (((symbol-function '+tasks--project-root) (lambda () temporary-file-directory))
               ((symbol-function '+tasks--project-test-command) (lambda (_) "bin/test"))
               ((symbol-function 'compile) (lambda (command &rest _) (setq compiled command))))
       (+tasks/test)))
    (ert-info ("SPC 5 t should run the jailed command as one shell-quoted line")
      (should (string-prefix-p "sandbox-exec -f /p/agent.sb " compiled))
      (should (string-suffix-p (shell-quote-argument "bin/test") compiled)))))

;;; task worktrees

(defun +tasks-test--current-branch (dir)
  (let ((default-directory dir))
    (car (+tasks--git-lines "branch" "--show-current"))))

(ert-deftest +tasks--worktree-path/one-dir-per-branch-under-the-repo ()
  (ert-info ("worktree-path should keep task worktrees under .worktrees, slashes flattened")
    (should (equal "/r/.worktrees/feat-a-b/" (+tasks--worktree-path "/r/" "feat/a-b")))))

(ert-deftest +tasks--parse-worktrees/maps-branches-to-dirs ()
  (ert-info ("parse-worktrees should map each checked-out branch to its dir and skip detached ones")
    (should (equal '(("main" . "/r/") ("feat/x" . "/r/.worktrees/feat-x/"))
                   (+tasks--parse-worktrees
                    '("worktree /r" "HEAD abc" "branch refs/heads/main"
                      "worktree /r/.worktrees/feat-x" "HEAD def" "branch refs/heads/feat/x"
                      "worktree /tmp/d" "HEAD 123" "detached"))))))

(ert-deftest +tasks--main-root/finds-the-repo-from-a-worktree ()
  (+tasks-test--in-git-repo
   (+tasks-test--git "worktree" "add" "-q" "-b" "feat/x" ".worktrees/feat-x")
   (ert-info ("main-root should find the repo from inside a task worktree, where tasks.org is not")
     (should (equal default-directory (+tasks--main-root (expand-file-name ".worktrees/feat-x/")))))
   (ert-info ("main-root should keep the main checkout")
     (should (equal default-directory (+tasks--main-root default-directory))))))

(ert-deftest +tasks--add-worktree/creates-a-branch-worktree-leaving-main-alone ()
  (+tasks-test--in-git-repo
   (let ((tree (+tasks--add-worktree default-directory "feat/x")))
     (ert-info ("add-worktree should create the planned worktree")
       (should (equal (+tasks--worktree-path default-directory "feat/x") tree))
       (should (file-directory-p tree)))
     (ert-info ("the worktree should be on the new branch")
       (should (equal "feat/x" (+tasks-test--current-branch tree))))
     (ert-info ("the main checkout, the one Emacs loads, should stay on main")
       (should (equal "main" (+tasks-test--current-branch default-directory))))
     (ert-info ("worktrees should be kept out of git status")
       (should-not (+tasks--git-lines "status" "--porcelain"))))))

(ert-deftest +tasks--add-worktree/reuses-what-a-branch-has ()
  (+tasks-test--in-git-repo
   (+tasks-test--git "branch" "feat/old")
   (let ((tree (+tasks--add-worktree default-directory "feat/old")))
     (ert-info ("add-worktree should check out an existing branch")
       (should (equal "feat/old" (+tasks-test--current-branch tree))))
     (ert-info ("add-worktree should return the worktree a branch already has instead of failing")
       (should (equal tree (+tasks--add-worktree default-directory "feat/old")))))
   (+tasks-test--git "switch" "-q" "-c" "feat/legacy")
   (ert-info ("a branch checked out in the main checkout by the old flow should be used where it is")
     (should (equal default-directory (+tasks--add-worktree default-directory "feat/legacy"))))))

(ert-deftest +tasks/start/hands-pi-the-task-worktree ()
  (let (handed
        (repo (file-name-as-directory (file-truename temporary-file-directory))))
    (+tasks-test--in-tasks-buffer "** IDEA Fix it\n"
      (setq default-directory repo)
      (+tasks-test--goto-heading "Fix it")
      (cl-letf (((symbol-function '+tasks--task-to-start) (lambda () (cons (current-buffer) (line-beginning-position))))
                ((symbol-function 'read-string) (lambda (_prompt &optional initial &rest _) (or initial "it works")))
                ((symbol-function '+tasks--branch-worktree-in) (lambda (&rest _) nil))
                ((symbol-function '+tasks--hand-off) (lambda (root &rest args) (setq handed (cons root args)))))
        (+tasks/start)))
    (ert-info ("start should open pi in the task's own worktree")
      (should (equal (+tasks--worktree-path repo "feat/fix-it") (car handed))))
    (ert-info ("start should review the repo, since the worktree only exists once the review allows it")
      (should (equal repo (plist-get (cdr handed) :review-root))))))

(ert-deftest +tasks--test-root/tests-the-task-worktree-from-tasks-org ()
  (cl-letf (((symbol-function '+tasks--linked-task-at-point)
             (lambda () (list :buffer (current-buffer) :branch "feat/x")))
            ((symbol-function '+tasks--branch-worktree-in) (lambda (_root branch) (and (equal branch "feat/x") "/r/.worktrees/feat-x/"))))
    (ert-info ("SPC 5 t on a task should test its worktree, not the main checkout")
      (should (equal "/r/.worktrees/feat-x/" (+tasks--test-root))))))

(ert-deftest +tasks--test-root/falls-back-to-the-project ()
  (cl-letf (((symbol-function '+tasks--linked-task-at-point) #'ignore)
            ((symbol-function '+tasks--project-root) (lambda () "/p/")))
    (ert-info ("SPC 5 t elsewhere should test the current project")
      (should (equal "/p/" (+tasks--test-root))))))

(defun +tasks-test--write-active-task (buffer heading branch)
  (with-current-buffer buffer
    (insert (+tasks--skeleton))
    (goto-char (point-min))
    (re-search-forward "^\\* Active\n")
    (insert (format "** TODO %s\n:PROPERTIES:\n:BRANCH: %s\n:END:\n" heading branch))
    (save-buffer)
    (org-mode)))

(defun +tasks-test--task-in (buffer heading)
  (with-current-buffer buffer
    (save-excursion
      (+tasks-test--goto-heading heading)
      (+tasks--task-plist))))

(ert-deftest +tasks/done/merges-the-worktree-and-removes-it ()
  (+tasks-test--in-git-repo
   (let* ((repo default-directory)
          (tree (+tasks--add-worktree repo "feat/x"))
          (buffer (find-file-noselect (expand-file-name "tasks.org" repo))))
     (unwind-protect
         (progn
           (let ((default-directory tree))
             (+tasks-test--commit-file "feature.txt" "done"))
           (+tasks-test--write-active-task buffer "Ship it" "feat/x")
           (cl-letf (((symbol-function '+tasks--work-task) (lambda () (+tasks-test--task-in buffer "Ship it")))
                     ((symbol-function '+tasks--project-test-command) (lambda (_) "true"))
                     ((symbol-function 'message) #'ignore))
             (+tasks/done))
           (ert-info ("done should merge the branch into main in the main checkout")
             (should (file-exists-p (expand-file-name "feature.txt" repo)))
             (should (equal "main" (+tasks-test--current-branch repo))))
           (ert-info ("done should remove the task worktree")
             (should-not (file-exists-p tree)))
           (ert-info ("done should delete the merged branch")
             (should-not (+tasks--git-ok-p "show-ref" "--verify" "--quiet" "refs/heads/feat/x")))
           (ert-info ("done should mark the task DONE")
             (should (equal "DONE" (with-current-buffer buffer
                                     (+tasks-test--goto-heading "Ship it")
                                     (org-get-todo-state))))))
       (with-current-buffer buffer (set-buffer-modified-p nil))
       (kill-buffer buffer)))))

(ert-deftest +tasks/done/refuses-a-worktree-with-uncommitted-work ()
  (+tasks-test--in-git-repo
   (let ((tree (+tasks--add-worktree default-directory "feat/x")))
     (with-temp-file (expand-file-name "forgotten_test.py" tree) (insert "x"))
     (cl-letf (((symbol-function '+tasks--work-task)
                (lambda () (list :buffer (current-buffer) :heading "X" :branch "feat/x"))))
       (ert-info ("done should stop when the worktree holds work nobody committed, untracked files included")
         (should-error (+tasks/done) :type 'user-error)))
     (ert-info ("a refused done should leave the worktree in place")
       (should (file-exists-p (expand-file-name "forgotten_test.py" tree)))))))

(ert-deftest +tasks--start-at/adds-no-worktree-without-an-active-heading ()
  (+tasks-test--in-git-repo
   (let ((repo default-directory)
         (buffer (find-file-noselect (expand-file-name "tasks.org" default-directory))))
     (unwind-protect
         (progn
           (with-current-buffer buffer
             (insert "#+TODO: IDEA TODO | DONE\n* Ideas\n** IDEA Fix it\n* Backlog\n")
             (save-buffer)
             (org-mode)
             (+tasks-test--goto-heading "Fix it")
             (ert-info ("start should stop with a user-error when tasks.org has no * Active")
               (should-error (+tasks--start-at (point-marker) repo "feat/fix-it" "it works") :type 'user-error)))
           (ert-info ("a refused start should not have added the worktree or the branch")
             (should-not (file-exists-p (+tasks--worktree-path repo "feat/fix-it")))
             (should-not (+tasks--git-ok-p "show-ref" "--verify" "--quiet" "refs/heads/feat/fix-it")))
           (ert-info ("a refused start should leave the task where it was")
             (should (with-current-buffer buffer (string-match-p "IDEA Fix it" (buffer-string))))))
       (with-current-buffer buffer (set-buffer-modified-p nil))
       (kill-buffer buffer)))))

(ert-deftest +tasks/start/stops-before-the-review-without-an-active-heading ()
  (let (handed)
    (with-temp-buffer
      (insert "#+TODO: IDEA TODO | DONE\n* Ideas\n** IDEA Fix it\n")
      (org-mode)
      (+tasks-test--goto-heading "Fix it")
      (cl-letf (((symbol-function '+tasks--task-to-start) (lambda () (cons (current-buffer) (line-beginning-position))))
                ((symbol-function 'read-string) (lambda (_prompt &optional initial &rest _) (or initial "ok")))
                ((symbol-function '+tasks--hand-off) (lambda (&rest _) (setq handed t))))
        (ert-info ("start should refuse at once when tasks.org has no * Active")
          (should-error (+tasks/start) :type 'user-error))))
    (ert-info ("start should not open the repo review for a task it cannot refile")
      (should-not handed))))

(ert-deftest +tasks--start-at/adds-the-worktree-and-keeps-main-checked-out ()
  (+tasks-test--in-git-repo
   (let ((repo default-directory)
         (buffer (find-file-noselect (expand-file-name "tasks.org" default-directory))))
     (unwind-protect
         (progn
           (with-current-buffer buffer
             (insert (replace-regexp-in-string "^\\* Ideas\n" "* Ideas\n** IDEA Fix it\n" (+tasks--skeleton)))
             (save-buffer)
             (org-mode)
             (+tasks-test--goto-heading "Fix it")
             (+tasks--start-at (point-marker) repo "feat/fix-it" "it works"))
           (ert-info ("start should put the branch in its own worktree")
             (should (equal "feat/fix-it" (+tasks-test--current-branch (+tasks--worktree-path repo "feat/fix-it")))))
           (ert-info ("start should leave the main checkout, the config Emacs loads, on main")
             (should (equal "main" (+tasks-test--current-branch repo))))
           (ert-info ("start should still link and promote the task")
             (should (equal '("TODO" "feat/fix-it")
                            (with-current-buffer buffer
                              (+tasks-test--goto-heading "Fix it")
                              (list (org-get-todo-state) (org-entry-get nil "BRANCH")))))))
       (with-current-buffer buffer (set-buffer-modified-p nil))
       (kill-buffer buffer)))))
