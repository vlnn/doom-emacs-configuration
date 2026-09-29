;;; test/ai-test.el -*- lexical-binding: t; -*-

(ert-deftest my/llama-server-model-ids/extracts-symbols ()
  (ert-info ("model-ids should intern every data[].id")
    (should (equal '(qwopus-coder gpt-oss-20b)
                   (my/llama-server-model-ids
                    '((data . (((id . "qwopus-coder")) ((id . "gpt-oss-20b"))))))))))

(ert-deftest my/gptel-annotate-number-lines/prefixes-1-based ()
  (ert-info ("number-lines should prefix each line with its 1-based number")
    (should (equal "1: a\n2: b" (my/gptel-annotate-number-lines "a\nb")))))

(ert-deftest my/gptel-annotate-parse/accepts-common-separators ()
  (ert-info ("parse should accept N: N. N) and drop unnumbered lines")
    (should (equal '((1 . "one") (2 . "two") (3 . "three"))
                   (my/gptel-annotate-parse "1: one\n 2. two\n3) three\nnoise")))))

(ert-deftest my/gptel-annotate-explanation/joins-duplicates ()
  (ert-info ("explanation should join every entry for line N")
    (should (equal "a b" (my/gptel-annotate-explanation 1 '((1 . "a") (2 . "x") (1 . "b")))))))

(ert-deftest my/gptel-annotate-render/comments-above-each-line ()
  (let ((comment-start "#"))
    (ert-info ("render should put a numbered comment above each line, matching its indent")
      (should (equal "# 1. first\nx = 1\n    # 2. second\n    y = 2"
                     (my/gptel-annotate-render "x = 1\n    y = 2"
                                               '((1 . "first") (2 . "second"))))))))

(ert-deftest my/gptel-annotate-render/leaves-unexplained-lines ()
  (let ((comment-start "#"))
    (ert-info ("render should leave lines without an explanation untouched")
      (should (equal "x\ny" (my/gptel-annotate-render "x\ny" nil))))))
