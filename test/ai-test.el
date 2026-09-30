;;; test/ai-test.el -*- lexical-binding: t; -*-

(ert-deftest +ai--llama-model-ids/extracts-symbols ()
  (ert-info ("model-ids should intern every data[].id")
    (should (equal '(qwopus-coder gpt-oss-20b)
                   (+ai--llama-model-ids
                    '((data . (((id . "qwopus-coder")) ((id . "gpt-oss-20b"))))))))))

(ert-deftest +ai--annotate-number-lines/prefixes-1-based ()
  (ert-info ("number-lines should prefix each line with its 1-based number")
    (should (equal "1: a\n2: b" (+ai--annotate-number-lines "a\nb")))))

(ert-deftest +ai--annotate-parse/accepts-common-separators ()
  (ert-info ("parse should accept N: N. N) and drop unnumbered lines")
    (should (equal '((1 . "one") (2 . "two") (3 . "three"))
                   (+ai--annotate-parse "1: one\n 2. two\n3) three\nnoise")))))

(ert-deftest +ai--annotate-explanation/joins-duplicates ()
  (ert-info ("explanation should join every entry for line N")
    (should (equal "a b" (+ai--annotate-explanation 1 '((1 . "a") (2 . "x") (1 . "b")))))))

(ert-deftest +ai--annotate-render/comments-above-each-line ()
  (let ((comment-start "#"))
    (ert-info ("render should put a numbered comment above each line, matching its indent")
      (should (equal "# 1. first\nx = 1\n    # 2. second\n    y = 2"
                     (+ai--annotate-render "x = 1\n    y = 2"
                                               '((1 . "first") (2 . "second"))))))))

(ert-deftest +ai--annotate-render/leaves-unexplained-lines ()
  (let ((comment-start "#"))
    (ert-info ("render should leave lines without an explanation untouched")
      (should (equal "x\ny" (+ai--annotate-render "x\ny" nil))))))
