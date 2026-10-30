;;; test/lib-test.el -*- lexical-binding: t; -*-

(ert-deftest +git-lines/nil-when-git-fails ()
  (let ((default-directory (file-name-as-directory (make-temp-file "lib-test" t))))
    (unwind-protect
        (progn
          (ert-info ("git outside a repo should give nil, not its stderr")
            (should-not (+git-lines "rev-parse" "HEAD")))
          (call-process "git" nil nil nil "init" "-q" "-b" "main")
          (ert-info ("git output should come back as lines")
            (should (equal '("main") (+git-lines "branch" "--show-current")))))
      (delete-directory default-directory t))))

(ert-deftest +main-checkout/keeps-what-is-not-a-repo ()
  (ert-info ("a directory that does not exist should be returned as it is, without running git there")
    (should (equal "/nowhere/" (+main-checkout "/nowhere/")))))

(ert-deftest +justfiles/matches-just-s-names-in-any-case ()
  (let ((root (file-name-as-directory (make-temp-file "lib-just" t))))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "Justfile" root) (insert "test:\n"))
          (with-temp-file (expand-file-name "notes.md" root) (insert ""))
          (ert-info ("Justfile should be found whatever its case; other files not")
            (should (equal (list (expand-file-name "Justfile" root)) (+justfiles root)))))
      (delete-directory root t))))
