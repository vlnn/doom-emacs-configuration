;;; test/tasks-test.el -*- lexical-binding: t; -*-

(defmacro +tasks-test--with-projects (names &rest body)
  "Run BODY with `root' bound to a temp dir, by its true name, holding project dirs NAMES."
  (declare (indent 1))
  `(let ((root (file-truename (make-temp-file "tasks-test" t))))
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
    (let ((tasks (file-truename (expand-file-name "a/tasks.org" root))))
      (+tasks-test--touch tasks)
      (ert-info ("agenda-files should keep the base and add each project's tasks.org once")
        (should (equal (list "~/org/" tasks)
                       (+tasks--agenda-files
                        '("~/org/")
                        (list (expand-file-name "a/" root) (expand-file-name "a/" root)))))))))

(defvar projectile-known-projects)

(defmacro +tasks-test--agenda (agenda-files &rest body)
  "Run BODY with project a, holding a tasks.org, known to projectile and AGENDA-FILES as yours."
  (declare (indent 1))
  `(+tasks-test--with-projects '("a")
     (let* ((tasks (file-truename (expand-file-name "a/tasks.org" root)))
            (projectile-known-projects (list (expand-file-name "a/" root)))
            (org-agenda-files ,agenda-files)
            (+tasks--added-agenda-files nil))
       (+tasks-test--touch tasks)
       ,@body)))

(ert-deftest +tasks--refresh-agenda-files/keeps-your-agenda-files ()
  (+tasks-test--agenda (list "~/org/" "/x/work.org")
    (+tasks--refresh-agenda-files)
    (ert-info ("refreshing should add each project's tasks.org after your own agenda files and dirs")
      (should (equal (list "~/org/" "/x/work.org" tasks) org-agenda-files)))))

(ert-deftest +tasks--refresh-agenda-files/drops-tasks-files-that-went-away ()
  (+tasks-test--agenda (list "~/org/")
    (+tasks--refresh-agenda-files)
    (delete-file tasks)
    (+tasks--refresh-agenda-files)
    (ert-info ("a tasks.org that no longer exists should leave the agenda, your own files should not")
      (should (equal (list "~/org/") org-agenda-files)))))

(ert-deftest +tasks--refresh-agenda-files/keeps-files-you-add-later ()
  (+tasks-test--agenda (list "~/org/")
    (+tasks--refresh-agenda-files)
    (push "/y/new.org" org-agenda-files)
    (+tasks--refresh-agenda-files)
    (ert-info ("an agenda file you add between refreshes should stay, with tasks.org listed once")
      (should (equal (list "/y/new.org" "~/org/" tasks) org-agenda-files)))))

(ert-deftest +tasks--refresh-agenda-files/starts-from-org-directory-when-you-have-none ()
  (+tasks-test--agenda nil
    (let ((org-directory "~/org/"))
      (+tasks--refresh-agenda-files)
      (ert-info ("with no agenda files of your own the agenda should keep org-directory, as before")
        (should (equal (list "~/org/" tasks) org-agenda-files))))))

(ert-deftest +tasks--refresh-agenda-files/works-before-projectile-loads ()
  (skip-unless (not (boundp 'projectile-known-projects)))
  (let ((org-agenda-files (list "~/org/"))
        (+tasks--added-agenda-files nil))
    (ert-info ("opening the agenda before projectile has loaded should keep your files and not signal")
      (should (equal (list "~/org/") (+tasks--refresh-agenda-files))))))

(ert-deftest +tasks--with-agenda-commands/adds-ours-replacing-same-keys ()
  (ert-info ("agenda commands should keep yours and replace only the ones under our keys")
    (should (equal '(("x" "Mine" todo "NEXT") ("b" "Board" todo "TODO"))
                   (+tasks--with-agenda-commands '(("x" "Mine" todo "NEXT") ("b" "Old board" todo "X"))
                                                 '(("b" "Board" todo "TODO")))))))

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
                   '(("#+TODO: IDEA(i) TODO(t) PLAN(p!) RED(r!) GREEN(g!) REVIEW(v!) WAIT(w!) | DONE(d!) DROPPED(x@)" . "declare the flow keywords, WAIT included")
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
                   `((,(org-get-todo-state) "PLAN" "switch IDEA to PLAN, the first in-flight stage")
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
      (should (string-match-p "^\\* Active :work:\n\\*\\* PLAN Keep me" (buffer-string))))))

(ert-deftest +tasks--start-entry/leaves-active-tasks-in-place-under-a-cookie ()
  (+tasks-test--in-tasks-buffer ""
    (re-search-forward "^\\* Active$")
    (insert " [0/2]\n** TODO First\n** TODO Second")
    (+tasks-test--goto-heading "First")
    (+tasks--start-entry "feat/first" "it works")
    (ert-info ("start-entry should not reorder a task that already sits under * Active [0/2]")
      (should (string-match-p "PLAN First\\(.\\|\n\\)*TODO Second" (buffer-string))))))

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
  (+tasks-test--in-tasks-buffer "** IDEA Idle\n** TODO Ready\n"
    (save-excursion (re-search-forward "^\\* Active\n")
                    (insert "** RED Failing\n** GREEN Passing\n** WAIT Blocked\n** DONE Shipped\n"))
    (ert-info ("open-tasks with the in-flight states should skip ideas, ready tasks and finished work")
      (should (equal '("Failing" "Passing" "Blocked") (mapcar #'car (+tasks--open-tasks +tasks-in-flight-states)))))))

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

(ert-deftest +tasks--justfile-text/reads-a-justfile-in-any-case ()
  (dolist (name '("justfile" "JUSTFILE" ".justfile"))
    (+tasks-test--with-projects '("p")
      (let ((dir (expand-file-name "p/" root)))
        (with-temp-file (expand-file-name name dir) (insert "test:\n\tbin/test\n"))
        (ert-info ((format "justfile-text should read %s, which just would run" name))
          (should (equal "test:\n\tbin/test\n" (+tasks--justfile-text dir))))))))

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

(defvar projectile-project-test-cmd nil)
(put 'projectile-project-test-cmd 'safe-local-variable #'stringp)

(ert-deftest +tasks--projectile-test-command/reads-the-tested-roots-dir-locals ()
  (+tasks-test--with-projects '("p")
    (let ((dir (expand-file-name "p/" root)))
      (with-temp-file (expand-file-name ".dir-locals.el" dir)
        (insert "((nil . ((projectile-project-test-cmd . \"bin/test\"))))"))
      (cl-letf (((symbol-function 'projectile-test-command) (lambda (_) projectile-project-test-cmd)))
        (with-temp-buffer
          (ert-info ("the test command should come from the tested root's dir-locals, whatever buffer done or test runs from")
            (should (equal "bin/test" (+tasks--projectile-test-command dir)))))))))

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
  (pcase-dolist (`(,existing . ,expected) '(("" . ("tasks.org" "tasks.org_archive" ".worktrees/" "PLAN.md"))
                                            ("# git ls-files\ntasks.org\n" . ("tasks.org_archive" ".worktrees/" "PLAN.md"))
                                            ("tasks.org\ntasks.org_archive\n.worktrees/\nPLAN.md\n" . ())))
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

(ert-deftest +tasks--work-task/acts-on-the-branches-row-at-point ()
  (+tasks-test--with-projects '("p")
    (let ((buffer (find-file-noselect (expand-file-name "p/tasks.org" root))))
      (unwind-protect
          (with-current-buffer buffer
            (insert (+tasks--skeleton))
            (goto-char (point-min))
            (re-search-forward "^\\* Active\n")
            (insert "** TODO Linked\n:PROPERTIES:\n:BRANCH: feat/x\n:END:\n")
            (re-search-forward "^#\\+BEGIN: branches\n")
            (insert "| feat/x | now | Linked | ahead 1 |\n")
            (save-buffer)
            (org-mode)
            (forward-line -1)
            (ert-info ("review, test and done on a branches row should act on the task linked to that row")
              (should (equal '("feat/x" "Linked")
                             (let ((task (+tasks--work-task)))
                               (list (plist-get task :branch) (plist-get task :heading)))))))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

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
                   `((,(org-get-todo-state) "PLAN" "promote only the started leaf")
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
               ((symbol-function 'compile) (lambda (command &rest _) (setq compiled command) (current-buffer))))
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

(ert-deftest +tasks--worktrees-dir-regexp/matches-the-worktree-dir-only ()
  (pcase-dolist (`(,dir . ,expected) '(("/r/.worktrees" . t) ("/r/sub/.worktrees" . t)
                                       ("/r/.worktrees-old" . nil) ("/r/my.worktrees" . nil)))
    (ert-info ((format "lsp should%s skip watching %s" (if expected "" " not") dir))
      (should (eq expected (and (string-match-p (+tasks--worktrees-dir-regexp) dir) t))))))

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

(ert-deftest +tasks/done/tests-the-branch-in-its-worktree ()
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
                     ((symbol-function '+tasks--project-test-command) (lambda (_) "test -e feature.txt"))
                     ((symbol-function 'message) #'ignore))
             (+tasks/done))
           (ert-info ("done should run the tests in the worktree, where the branch's own files are, not on main")
             (should (file-exists-p (expand-file-name "feature.txt" repo)))))
       (with-current-buffer buffer (set-buffer-modified-p nil))
       (kill-buffer buffer)))))

(ert-deftest +tasks--run-tests-or-confirm/runs-the-command-in-the-tested-root ()
  (let (ran-in)
    (cl-letf (((symbol-function '+tasks--project-test-command) (lambda (_) "bin/test"))
              ((symbol-function '+ai--jailed) (lambda (_root command &rest _) command))
              ((symbol-function 'message) #'ignore)
              ((symbol-function 'call-process) (lambda (&rest _) (setq ran-in default-directory) 0)))
      (let ((default-directory "/elsewhere/"))
        (+tasks--run-tests-or-confirm "/r/.worktrees/feat-x/")))
    (ert-info ("run-tests-or-confirm should start the command in the root it tests")
      (should (equal "/r/.worktrees/feat-x/" ran-in)))))

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
             (should (equal '("PLAN" "feat/fix-it")
                            (with-current-buffer buffer
                              (+tasks-test--goto-heading "Fix it")
                              (list (org-get-todo-state) (org-entry-get nil "BRANCH")))))))
       (with-current-buffer buffer (set-buffer-modified-p nil))
       (kill-buffer buffer)))))

;;; stages: the commands move tasks along the board

(defun +tasks-test--state-of (buffer heading)
  (with-current-buffer buffer
    (+tasks-test--goto-heading heading)
    (list (org-get-todo-state) (org-entry-get nil "REASON"))))

(ert-deftest +tasks--start-entry/promotes-a-ready-todo-to-plan ()
  (+tasks-test--in-tasks-buffer "** TODO Ready one\n"
    (+tasks-test--goto-heading "Ready one")
    (+tasks--start-entry "feat/ready-one" nil)
    (ert-info ("start-entry should move a ready TODO into PLAN too, so TODO only ever means ready")
      (should (equal "PLAN" (car (+tasks-test--state-of (current-buffer) "Ready one")))))))

(ert-deftest +tasks--set-state/moves-the-task-and-keeps-a-wait-reason ()
  (pcase-dolist (`(,state ,reason ,expected ,why)
                 '(("RED" nil ("RED" nil) "switch to the state")
                   ("WAIT" "upstream fix" ("WAIT" "upstream fix") "keep the reason while waiting")))
    (+tasks-test--in-tasks-buffer "** PLAN Thing\n"
      (+tasks-test--goto-heading "Thing")
      (+tasks--set-state (+tasks--task-plist) state :reason reason)
      (ert-info ((format "set-state should %s" why))
        (should (equal expected (+tasks-test--state-of (current-buffer) "Thing")))))))

(ert-deftest +tasks--set-state/drops-the-reason-when-the-task-stops-waiting ()
  (+tasks-test--in-tasks-buffer "** WAIT Thing\n:PROPERTIES:\n:REASON: upstream fix\n:END:\n"
    (+tasks-test--goto-heading "Thing")
    (+tasks--set-state (+tasks--task-plist) "RED")
    (ert-info ("set-state should drop a stale reason once the task is moving again")
      (should (equal '("RED" nil) (+tasks-test--state-of (current-buffer) "Thing"))))))

(ert-deftest +tasks--set-state/moves-the-task-wherever-point-is ()
  (+tasks-test--in-tasks-buffer "** PLAN First\n** PLAN Second\n"
    (+tasks-test--goto-heading "Second")
    (let ((task (+tasks--task-plist)))
      (goto-char (point-min))
      (+tasks--set-state task "GREEN"))
    (ert-info ("set-state should act on the task it was given, not on the heading at point")
      (should (equal '("PLAN" "GREEN")
                     (list (car (+tasks-test--state-of (current-buffer) "First"))
                           (car (+tasks-test--state-of (current-buffer) "Second"))))))))

(ert-deftest +tasks-file-mode/asks-why-when-you-wait-by-hand ()
  (+tasks-test--in-tasks-buffer "** RED Thing\n"
    (+tasks-file-mode 1)
    (+tasks-test--goto-heading "Thing")
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "waiting on a decision")))
      (org-todo "WAIT"))
    (ert-info ("entering WAIT by hand should ask for and store the reason")
      (should (equal '("WAIT" "waiting on a decision") (+tasks-test--state-of (current-buffer) "Thing"))))
    (+tasks-test--goto-heading "Thing")
    (org-todo "RED")
    (ert-info ("leaving WAIT by hand should clear the reason")
      (should (equal '("RED" nil) (+tasks-test--state-of (current-buffer) "Thing"))))))

(ert-deftest +tasks-file-mode/keeps-a-reason-the-flow-already-gave ()
  (+tasks-test--in-tasks-buffer "** RED Thing\n"
    (+tasks-file-mode 1)
    (+tasks-test--goto-heading "Thing")
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (error "should not ask"))))
      (+tasks--set-state (+tasks--task-plist) "WAIT" :reason "main cannot fast-forward"))
    (ert-info ("an automatic WAIT should not ask again for a reason it already has")
      (should (equal "main cannot fast-forward" (cadr (+tasks-test--state-of (current-buffer) "Thing")))))))

(ert-deftest +tasks/wait/parks-the-task-with-your-reason ()
  (+tasks-test--in-tasks-buffer "** GREEN Thing\n"
    (+tasks-test--goto-heading "Thing")
    (cl-letf (((symbol-function '+tasks--work-task) (let ((task (+tasks--task-plist))) (lambda () task)))
              ((symbol-function 'read-string) (lambda (&rest _) "needs the API key")))
      (+tasks/wait))
    (ert-info ("SPC 5 w should move the task to WAIT with the reason you give")
      (should (equal '("WAIT" "needs the API key") (+tasks-test--state-of (current-buffer) "Thing"))))))

(ert-deftest +tasks--test-verdict/reads-the-compilation-outcome ()
  (pcase-dolist (`(,msg . ,expected) '(("finished\n" . "GREEN")
                                       ("exited abnormally with code 1\n" . "RED")
                                       ("interrupt\n" . "RED")))
    (ert-info ((format "a run ending %S should leave the task %s" msg expected))
      (should (equal expected (+tasks--test-verdict msg))))))

(ert-deftest +tasks--mark-compiled-tests/moves-the-tested-task ()
  (+tasks-test--in-tasks-buffer "** PLAN Thing\n:PROPERTIES:\n:BRANCH: feat/thing\n:END:\n"
    (+tasks-test--goto-heading "Thing")
    (let ((task (+tasks--task-plist)))
      (with-temp-buffer
        (setq-local +tasks--compiled-task task)
        (+tasks--mark-compiled-tests (current-buffer) "finished\n")))
    (ert-info ("a compilation started by SPC 5 t should move its task on finishing")
      (should (equal "GREEN" (car (+tasks-test--state-of (current-buffer) "Thing")))))))

(ert-deftest +tasks--mark-compiled-tests/ignores-other-compilations ()
  (+tasks-test--in-tasks-buffer "** PLAN Thing\n"
    (with-temp-buffer
      (+tasks--mark-compiled-tests (current-buffer) "finished\n"))
    (ert-info ("a compilation that no task started should change nothing")
      (should (equal "PLAN" (car (+tasks-test--state-of (current-buffer) "Thing")))))))

(ert-deftest +tasks--mark-compiled-tests/lets-a-killed-tasks-buffer-go ()
  (let ((task (with-temp-buffer (insert "* PLAN Gone\n") (org-mode) (goto-char (point-min)) (+tasks--task-plist))))
    (with-temp-buffer
      (setq-local +tasks--compiled-task task)
      (ert-info ("a run finishing after tasks.org was killed should not error in the compilation hook")
        (should-not (+tasks--mark-compiled-tests (current-buffer) "finished\n"))))))

(ert-deftest +tasks/test/remembers-the-task-in-the-compilation-buffer ()
  (+tasks-test--in-tasks-buffer "** PLAN Thing\n:PROPERTIES:\n:BRANCH: feat/thing\n:END:\n"
    (+tasks-test--goto-heading "Thing")
    (let ((task (+tasks--task-plist))
          (compilation (generate-new-buffer "*compilation*")))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function '+tasks--linked-task-at-point) (lambda () task))
                      ((symbol-function '+tasks--branch-worktree-in) (lambda (&rest _) temporary-file-directory))
                      ((symbol-function '+tasks--project-test-command) (lambda (_) "bin/test"))
                      ((symbol-function '+ai--jailed) (lambda (_root command &rest _) command))
                      ((symbol-function 'compile) (lambda (&rest _) compilation)))
              (+tasks/test))
            (ert-info ("SPC 5 t should leave the task in the compilation buffer for the finish hook")
              (should (eq task (buffer-local-value '+tasks--compiled-task compilation))))
            (ert-info ("SPC 5 t should hook the finish function that moves the task")
              (should (memq #'+tasks--mark-compiled-tests compilation-finish-functions))))
        (kill-buffer compilation)))))

(ert-deftest +tasks/review/moves-the-task-to-review ()
  (+tasks-test--in-tasks-buffer "** GREEN Thing\n:PROPERTIES:\n:BRANCH: feat/thing\n:END:\n"
    (+tasks-test--goto-heading "Thing")
    (cl-letf (((symbol-function '+tasks--work-task) (let ((task (+tasks--task-plist))) (lambda () task)))
              ((symbol-function '+tasks--default-branch) (lambda () "main"))
              ((symbol-function '+tasks--changed-files) #'ignore)
              ((symbol-function 'magit-diff-range) #'ignore)
              ((symbol-function 'message) #'ignore))
      (+tasks/review))
    (ert-info ("opening the review diff should move the task to REVIEW")
      (should (equal "REVIEW" (car (+tasks-test--state-of (current-buffer) "Thing")))))))

(ert-deftest +tasks/done/leaves-a-failing-branch-red ()
  (+tasks-test--in-git-repo
   (let* ((repo default-directory)
          (tree (+tasks--add-worktree repo "feat/x"))
          (buffer (find-file-noselect (expand-file-name "tasks.org" repo))))
     (unwind-protect
         (progn
           (+tasks-test--write-active-task buffer "Ship it" "feat/x")
           (cl-letf (((symbol-function '+tasks--work-task) (lambda () (+tasks-test--task-in buffer "Ship it")))
                     ((symbol-function '+tasks--project-test-command) (lambda (_) "false"))
                     ((symbol-function 'display-buffer) #'ignore)
                     ((symbol-function 'message) #'ignore))
             (should-error (+tasks/done) :type 'user-error))
           (ert-info ("done should mark a task RED when its tests fail, and merge nothing")
             (should (equal "RED" (car (+tasks-test--state-of buffer "Ship it"))))
             (should (file-exists-p tree))))
       (with-current-buffer buffer (set-buffer-modified-p nil))
       (kill-buffer buffer)))))

(ert-deftest +tasks/done/parks-a-task-whose-merge-it-cannot-finish ()
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
                     ((symbol-function '+tasks--catch-up)
                      (lambda (branch main) (user-error "%s is merged on origin, but %s cannot fast-forward to it" branch main)))
                     ((symbol-function 'message) #'ignore))
             (should-error (+tasks/done) :type 'user-error))
           (ert-info ("done should park a task it cannot merge in WAIT, with the obstacle as the reason")
             (should (equal '("WAIT" "feat/x is merged on origin, but main cannot fast-forward to it")
                            (+tasks-test--state-of buffer "Ship it")))))
       (with-current-buffer buffer (set-buffer-modified-p nil))
       (kill-buffer buffer)))))

(ert-deftest +tasks--agenda-custom-commands/board-walks-the-pipeline-top-down ()
  (let ((board (cadr (cdr (assoc "b" +tasks--agenda-custom-commands)))))
    (ert-info ("the board should read as the pipeline, ideas at the top, finished work piling at the bottom")
      (should (equal '("IDEA" "TODO" "PLAN" "RED" "GREEN" "REVIEW" "WAIT" "DONE|DROPPED")
                     (mapcar #'cadr board))))
    (ert-info ("the board should keep ready tasks and ideas to leaves")
      (should (equal '(t t)
                     (mapcar (lambda (state)
                               (and (alist-get 'org-agenda-skip-function
                                               (nth 1 (assoc state (mapcar #'cdr board))))
                                    t))
                             '("TODO" "IDEA")))))))

(ert-deftest +tasks--agenda-custom-commands/board-settings-survive-the-agenda-eval ()
  (let ((board (cadr (cdr (assoc "b" +tasks--agenda-custom-commands)))))
    (dolist (block board)
      (pcase-dolist (`(,variable ,form) (nth 2 block))
        (ert-info ((format "the %s block's %s should evaluate as the agenda evaluates settings" (cadr block) variable))
          (should (eval form t)))))
    (ert-info ("the board's prefix format should come out as the alist the agenda wants")
      (should (equal +tasks--board-prefix-format
                     (eval (cadr (assq 'org-agenda-prefix-format (nth 2 (car board)))) t))))))

(ert-deftest +tasks--agenda-prefix/shows-the-branch-or-the-wait-reason ()
  (+tasks-test--in-tasks-buffer "** WAIT Parked\n:PROPERTIES:\n:BRANCH: feat/parked\n:REASON: API key\n:END:\n** RED Running\n:PROPERTIES:\n:BRANCH: feat/running\n:END:\n** IDEA Loose\n"
    (pcase-dolist (`(,heading . ,expected) '(("Parked" . "API key") ("Running" . "feat/running") ("Loose" . "")))
      (+tasks-test--goto-heading heading)
      (ert-info ((format "the board prefix of %s should read %S" heading expected))
        (should (equal expected (+tasks--agenda-prefix)))))))

(ert-deftest +tasks--agenda-prefix/keeps-to-its-column ()
  (+tasks-test--in-tasks-buffer "** RED Long\n:PROPERTIES:\n:BRANCH: feat/add-separate-keybinding-for-all-tasks\n:END:\n"
    (+tasks-test--goto-heading "Long")
    (ert-info ("a branch longer than the board column should be cut with an ellipsis, not run into the keyword")
      (should (equal "feat/add-separate-keybi…" (+tasks--agenda-prefix))))))

(ert-deftest +tasks--board-prefix-format/separates-the-column-from-the-heading ()
  (ert-info ("the prefix column should end in a space so a full-width value never touches the keyword")
    (should (string-suffix-p "(+tasks--agenda-prefix) " (cdr (assq 'todo +tasks--board-prefix-format))))))

(ert-deftest +tasks/menu-file/binds-wait ()
  (let ((text (with-temp-buffer
                (insert-file-contents (expand-file-name "modules/my/tasks/menu.el" default-directory))
                (buffer-string))))
    (ert-info ("the menu should bind w to parking a task with a reason")
      (should (string-search "(\"w\" \"wait: park with a reason\" +tasks/wait)" text)))))

;;; board scope: this project or every project

(ert-deftest +tasks--project-filter/selects-the-category-the-project-files-carry ()
  (ert-info ("the project filter should be the positive category filter the agenda applies")
    (should (equal '("+doom-emacs-configuration")
                   (+tasks--project-filter "/home/va/doom-emacs-configuration/")))))

(ert-deftest +tasks/board/applies-the-scope-it-is-opened-with ()
  (pcase-dolist (`(,scope . ,expected) '((project . ("+doom")) (all . nil)))
    (let ((+tasks-board-scope scope)
          (org-agenda-category-filter '("+stale"))
          (steps nil))
      (cl-letf (((symbol-function '+tasks--project-root) (lambda () "/p/doom/"))
                ((symbol-function 'org-agenda) #'ignore)
                ((symbol-function 'org-agenda-filter-show-all-cat)
                 (lambda () (setq org-agenda-category-filter nil) (push 'shown-all steps)))
                ((symbol-function 'org-agenda-filter-apply)
                 (lambda (filter type) (setq org-agenda-category-filter filter) (push (list filter type) steps))))
        (+tasks/board))
      (ert-info ((format "a board opened in scope %s should first drop any earlier category filter" scope))
        (should (eq 'shown-all (car (last steps)))))
      (ert-info ((format "a board opened in scope %s should end up filtering categories to %S" scope expected))
        (should (equal expected org-agenda-category-filter))))))

(ert-deftest +tasks/toggle-board-scope/flips-and-reopens ()
  (let ((+tasks-board-scope 'all)
        reopened)
    (cl-letf (((symbol-function '+tasks/board) (lambda () (setq reopened +tasks-board-scope))))
      (+tasks/toggle-board-scope)
      (ert-info ("toggling from all should narrow the board to this project and reopen it")
        (should (eq 'project reopened)))
      (+tasks/toggle-board-scope)
      (ert-info ("toggling again should widen the board back to every project")
        (should (eq 'all reopened))))))

(ert-deftest +tasks/menu-file/binds-the-board-scope-toggle ()
  (let ((text (with-temp-buffer
                (insert-file-contents (expand-file-name "modules/my/tasks/menu.el" default-directory))
                (buffer-string))))
    (ert-info ("the menu should bind O to flipping the board between this project and all")
      (should (string-search "(\"O\" \"board: this project / all\" +tasks/toggle-board-scope)" text)))))

;;; next: one key moves the task at point along

(ert-deftest +tasks--next-step/maps-each-state-to-the-command-that-moves-it ()
  (pcase-dolist (`(,state . ,command) '(("IDEA" . +tasks/start) ("TODO" . +tasks/start)
                                        ("PLAN" . +tasks/test) ("RED" . +tasks/test)
                                        ("GREEN" . +tasks/review) ("REVIEW" . +tasks/done)
                                        ("WAIT" . +tasks/unpark)
                                        ("DONE" . org-archive-subtree) ("DROPPED" . org-archive-subtree)))
    (ert-info ((format "next on a %s task should run %s" state command))
      (should (eq command (+tasks--next-step state))))))

(ert-deftest +tasks--next-step/refuses-a-heading-with-no-state ()
  (ert-info ("next on a plain heading should have nothing to do"))
  (should-not (+tasks--next-step nil)))

(ert-deftest +tasks/next/runs-the-step-for-the-heading-at-point ()
  (+tasks-test--in-tasks-buffer "** GREEN Thing\n:PROPERTIES:\n:BRANCH: feat/thing\n:END:\n"
    (setq buffer-file-name (expand-file-name "tasks.org" temporary-file-directory))
    (+tasks-test--goto-heading "Thing")
    (let (ran)
      (cl-letf (((symbol-function '+tasks/review) (lambda () (setq ran (org-get-heading t t t t)))))
        (+tasks/next))
      (ert-info ("next on a GREEN heading should review it")
        (should (equal "Thing" ran))))
    (setq buffer-file-name nil)))

(ert-deftest +tasks/next/follows-a-board-row-to-its-heading ()
  (+tasks-test--in-tasks-buffer "** PLAN Thing\n:PROPERTIES:\n:BRANCH: feat/thing\n:END:\n"
    (setq buffer-file-name (expand-file-name "tasks.org" temporary-file-directory))
    (+tasks-test--goto-heading "Thing")
    (let ((marker (point-marker))
          ran)
      (with-temp-buffer
        (insert (propertize " tasks: PLAN Thing" 'org-hd-marker marker))
        (org-agenda-mode)
        (goto-char (point-min))
        (cl-letf (((symbol-function '+tasks/test) (lambda () (setq ran (list (buffer-name) (org-get-heading t t t t))))))
          (+tasks/next)))
      (ert-info ("next on a board row should act in tasks.org at that row's heading")
        (should (equal (list (buffer-name) "Thing") ran))))
    (setq buffer-file-name nil)))

(ert-deftest +tasks/next/says-when-there-is-nothing-to-advance ()
  (+tasks-test--in-tasks-buffer ""
    (setq buffer-file-name (expand-file-name "tasks.org" temporary-file-directory))
    (+tasks-test--goto-heading "Backlog")
    (ert-info ("next on a structural heading should stop with a user error")
      (should-error (+tasks/next) :type 'user-error))
    (setq buffer-file-name nil)))

(ert-deftest +tasks/unpark/returns-a-waiting-task-to-plan-and-resumes-it ()
  (+tasks-test--in-tasks-buffer "** WAIT Thing\n:PROPERTIES:\n:BRANCH: feat/thing\n:REASON: API key\n:END:\n"
    (+tasks-test--goto-heading "Thing")
    (let (resumed)
      (cl-letf (((symbol-function '+tasks--work-task) (let ((task (+tasks--task-plist))) (lambda () task)))
                ((symbol-function '+tasks/resume) (lambda () (setq resumed t))))
        (+tasks/unpark))
      (ert-info ("unpark should put the task back in PLAN with its reason gone, and reopen its pi session")
        (should (equal '("PLAN" nil) (+tasks-test--state-of (current-buffer) "Thing")))
        (should resumed)))))

(ert-deftest +tasks/menu-file/binds-next ()
  (let ((text (with-temp-buffer
                (insert-file-contents (expand-file-name "modules/my/tasks/menu.el" default-directory))
                (buffer-string))))
    (ert-info ("the menu should bind n to advancing the task at point")
      (should (string-search "(\"n\" \"next: advance task at point\" +tasks/next)" text)))))

;;; abort: throw the agent's work away and put the task back where it was

(ert-deftest +tasks--state-before-start/reads-where-the-task-came-from ()
  (pcase-dolist (`(,log . ,expected)
                 '(("- State \"PLAN\"       from \"IDEA\"       [2026-10-17 Sat 10:00]\n" . "IDEA")
                   ("- State \"RED\"        from \"PLAN\"       [2026-10-17 Sat 11:00]\n- State \"PLAN\"       from \"TODO\"       [2026-10-17 Sat 10:00]\n" . "TODO")
                   (":LOGBOOK:\n- State \"PLAN\"       from \"IDEA\"       [2026-10-17 Sat 10:00]\n:END:\n" . "IDEA")
                   ("" . "TODO")))
    (+tasks-test--in-tasks-buffer (concat "** RED Thing\n" log "** IDEA Next\n- State \"PLAN\"       from \"IDEA\"       [2026-10-17 Sat 09:00]\n")
      (+tasks-test--goto-heading "Thing")
      (ert-info ((format "with this log an aborted task should go back to %s" expected))
        (should (equal expected (+tasks--state-before-start)))))))

(ert-deftest +tasks--abort-entry/unlinks-and-restores-keeping-what-you-wrote ()
  (+tasks-test--in-tasks-buffer "** WAIT Thing\n:PROPERTIES:\n:BRANCH: feat/thing\n:ACCEPT: it works\n:MODEL: claude\n:REASON: stuck\n:END:\n- State \"PLAN\"       from \"IDEA\"       [2026-10-17 Sat 10:00]\n"
    (+tasks-test--goto-heading "Thing")
    (+tasks--abort-entry)
    (+tasks-test--goto-heading "Thing")
    (pcase-dolist (`(,property . ,expected) '(("BRANCH" . nil) ("REASON" . nil) ("ACCEPT" . "it works") ("MODEL" . "claude")))
      (ert-info ((format "abort should leave %s as %S" property expected))
        (should (equal expected (org-entry-get nil property)))))
    (ert-info ("abort should return the task to the state it was started from")
      (should (equal "IDEA" (org-get-todo-state))))))

(ert-deftest +tasks/abort/drops-branch-and-worktree-and-puts-the-task-back ()
  (+tasks-test--in-git-repo
   (let* ((repo default-directory)
          (tree (+tasks--add-worktree repo "feat/x"))
          (buffer (find-file-noselect (expand-file-name "tasks.org" repo)))
          stopped)
     (unwind-protect
         (progn
           (let ((default-directory tree))
             (+tasks-test--commit-file "half.txt" "unfinished")
             (with-temp-file (expand-file-name "dirty.txt" tree) (insert "x")))
           (+tasks-test--write-active-task buffer "Risky idea" "feat/x")
           (cl-letf (((symbol-function '+tasks--work-task) (lambda () (+tasks-test--task-in buffer "Risky idea")))
                     ((symbol-function '+ai--stop-pilish-session) (lambda (root session) (setq stopped (list root session))))
                     ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                     ((symbol-function 'message) #'ignore))
             (+tasks/abort))
           (ert-info ("abort should quit the task's pi session before touching its worktree")
             (should (equal (list tree "feat/x") stopped)))
           (ert-info ("abort should remove the worktree, uncommitted work included")
             (should-not (file-exists-p tree)))
           (ert-info ("abort should delete the branch, unmerged commits included")
             (should-not (+tasks--git-ok-p "show-ref" "--verify" "--quiet" "refs/heads/feat/x")))
           (ert-info ("abort should leave main as it was")
             (should-not (file-exists-p (expand-file-name "half.txt" repo)))
             (should (equal "main" (+tasks-test--current-branch repo))))
           (ert-info ("abort should put the task back as a ready TODO with no branch")
             (should (equal '("TODO" nil)
                            (with-current-buffer buffer
                              (+tasks-test--goto-heading "Risky idea")
                              (list (org-get-todo-state) (org-entry-get nil "BRANCH")))))))
       (with-current-buffer buffer (set-buffer-modified-p nil))
       (kill-buffer buffer)))))

(ert-deftest +tasks/abort/does-nothing-unless-you-say-yes ()
  (+tasks-test--in-git-repo
   (let* ((repo default-directory)
          (tree (+tasks--add-worktree repo "feat/x"))
          (buffer (find-file-noselect (expand-file-name "tasks.org" repo))))
     (unwind-protect
         (progn
           (+tasks-test--write-active-task buffer "Keep" "feat/x")
           (cl-letf (((symbol-function '+tasks--work-task) (lambda () (+tasks-test--task-in buffer "Keep")))
                     ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
             (should-error (+tasks/abort) :type 'user-error))
           (ert-info ("a declined abort should keep the worktree and the branch")
             (should (file-exists-p tree))
             (should (+tasks--git-ok-p "show-ref" "--verify" "--quiet" "refs/heads/feat/x"))))
       (with-current-buffer buffer (set-buffer-modified-p nil))
       (kill-buffer buffer)))))

(ert-deftest +tasks/abort/refuses-a-branch-that-is-already-merged ()
  (+tasks-test--in-git-repo
   (let* ((repo default-directory)
          (buffer (find-file-noselect (expand-file-name "tasks.org" repo))))
     (+tasks-test--git "branch" "feat/x")
     (unwind-protect
         (progn
           (+tasks-test--write-active-task buffer "Shipped" "feat/x")
           (cl-letf (((symbol-function '+tasks--work-task) (lambda () (+tasks-test--task-in buffer "Shipped")))
                     ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
             (ert-info ("abort should refuse a branch main already contains; that is done's job")
               (should-error (+tasks/abort) :type 'user-error))))
       (with-current-buffer buffer (set-buffer-modified-p nil))
       (kill-buffer buffer)))))

(ert-deftest +tasks/menu-file/binds-abort ()
  (let ((text (with-temp-buffer
                (insert-file-contents (expand-file-name "modules/my/tasks/menu.el" default-directory))
                (buffer-string))))
    (ert-info ("the menu should bind x to aborting the task")
      (should (string-search "(\"x\" \"abort: drop branch, worktree, back to ready\" +tasks/abort)" text)))))

(ert-deftest +tasks/abort/keeps-the-dropped-commits-under-a-backup-ref ()
  (+tasks-test--in-git-repo
   (let* ((repo default-directory)
          (tree (+tasks--add-worktree repo "feat/x"))
          (buffer (find-file-noselect (expand-file-name "tasks.org" repo)))
          tip)
     (unwind-protect
         (progn
           (let ((default-directory tree))
             (+tasks-test--commit-file "half.txt" "unfinished")
             (setq tip (car (+tasks--git-lines "rev-parse" "HEAD"))))
           (+tasks-test--write-active-task buffer "Risky idea" "feat/x")
           (cl-letf (((symbol-function '+tasks--work-task) (lambda () (+tasks-test--task-in buffer "Risky idea")))
                     ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                     ((symbol-function 'message) #'ignore))
             (+tasks/abort))
           (ert-info ("abort should park the branch tip under refs/aborted so the agent's work can still be read")
             (should (equal tip (car (+tasks--git-lines "rev-parse" "refs/aborted/feat/x"))))))
       (with-current-buffer buffer (set-buffer-modified-p nil))
       (kill-buffer buffer)))))
