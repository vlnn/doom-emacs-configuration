;;; test/snippets-test.el -*- lexical-binding: t; -*-

(ert-deftest +snippets--min-indent/ignores-blank-lines ()
  (ert-info ("min-indent should skip whitespace-only lines")
    (should (equal 4 (+snippets--min-indent '("    a" "" "      b" "  "))))))

(ert-deftest +snippets--min-indent/nil-when-all-blank ()
  (ert-info ("min-indent should be nil when nothing is indented")
    (should-not (+snippets--min-indent '("" "   ")))))

(ert-deftest +snippets--reindent-line/shifts-indent ()
  (ert-info ("reindent-line should replace FROM columns with TO spaces")
    (should (equal "      x" (+snippets--reindent-line "  x" 2 6)))))

(ert-deftest +snippets--reindent-line/leaves-short-lines ()
  (ert-info ("reindent-line should leave lines shorter than FROM untouched")
    (should (equal "" (+snippets--reindent-line "" 4 8)))))

(ert-deftest +snippets-body/empty-selection-gives-indent ()
  (cl-letf (((symbol-function 'yas-selected-text) (lambda () nil)))
    (ert-info ("body should be an INDENT-wide blank when nothing is selected")
      (should (equal "    " (+snippets-body 4))))))

(ert-deftest +snippets-body/reindents-selection-block ()
  (cl-letf (((symbol-function 'yas-selected-text)
             (lambda () "  foo()\n\n    bar()")))
    (ert-info ("body should shift the block so its shallowest line sits at INDENT")
      (should (equal "        foo()\n\n          bar()" (+snippets-body 8))))))
