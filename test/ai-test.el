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

(defmacro +ai-test--in-buffer (text &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (emacs-lisp-mode)
     (insert ,text)
     ,@body))

(ert-deftest +ai--text-at-point/current-line-by-default ()
  (+ai-test--in-buffer "(defun f ()\n  (+ 1 2))\n"
    (goto-char 15)
    (ert-info ("without a region the current line should be used, sans newline")
      (should (equal "  (+ 1 2))" (+ai--text-at-point))))))

(ert-deftest +ai--text-at-point/whole-defun-on-request ()
  (+ai-test--in-buffer "(defun f ()\n  (+ 1 2))\n"
    (goto-char 15)
    (ert-info ("WHOLE-DEFUN should expand to the enclosing defun")
      (should (equal "(defun f ()\n  (+ 1 2))" (+ai--text-at-point t))))))

(ert-deftest +ai--text-at-point/region-wins ()
  (+ai-test--in-buffer "abc\ndef\n"
    (set-mark 1)
    (goto-char 8)
    (activate-mark)
    (let ((transient-mark-mode t))
      (ert-info ("an active region should be used verbatim, over line or defun")
        (should (equal "abc\ndef" (+ai--text-at-point t)))))))

(ert-deftest +ai/annotate/loads-gptel-before-asking ()
  (+ai-test--in-buffer "(f)\n"
    (let (steps)
      (cl-letf (((symbol-function 'require) (lambda (feature &rest _) (push feature steps)))
                ((symbol-function 'gptel-request) (lambda (&rest _) (push 'asked steps)))
                ((symbol-function '+ai--annotate-buffer) (lambda (_) (current-buffer)))
                ((symbol-function 'pop-to-buffer) #'ignore))
        (+ai/annotate))
      (ert-info ("gptel should be loaded before its request function is called, since only loading defines it")
        (should (equal '(gptel asked) (reverse steps)))))))

(ert-deftest +ai--bounds-at-point/line-defun-region ()
  (+ai-test--in-buffer "(defun f ()\n  (+ 1 2))\n"
    (goto-char 15)
    (ert-info ("without a region the bounds should be the current line")
      (should (equal '(13 . 23) (+ai--bounds-at-point))))
    (ert-info ("WHOLE-DEFUN should give the enclosing defun")
      (should (equal '(1 . 23) (+ai--bounds-at-point t))))
    (set-mark 3)
    (goto-char 8)
    (activate-mark)
    (let ((transient-mark-mode t))
      (ert-info ("an active region should win")
        (should (equal '(3 . 8) (+ai--bounds-at-point t)))))))

;;; annotations beside the code

(defun +ai-test--annotations ()
  (mapcar (lambda (o) (cons (line-number-at-pos (overlay-start o))
                            (substring-no-properties (overlay-get o 'after-string) 1)))
          (sort (seq-filter (lambda (o) (overlay-get o '+ai-annotate)) (overlays-in (point-min) (point-max)))
                (lambda (a b) (< (overlay-start a) (overlay-start b))))))

(ert-deftest +ai--annotate-complete/keeps-only-finished-lines ()
  (ert-info ("a stream should be read up to its last newline")
    (should (equal "1: a\n2: b\n" (+ai--annotate-complete "1: a\n2: b\n3: c"))))
  (ert-info ("a stream with no newline yet should read as nothing")
    (should (equal "" (+ai--annotate-complete "1: a")))))

(ert-deftest +ai--annotate-groups/offsets-to-buffer-lines-and-joins ()
  (ert-info ("groups should be buffer lines from START with duplicates joined")
    (should (equal '((5 . "a b") (6 . "c"))
                   (mapcar (lambda (g) (cons (car g) (substring-no-properties (cdr g))))
                           (+ai--annotate-groups '((1 . "a") (2 . "c") (1 . "b")) 5)))))
  (ert-info ("every group should wear the annotation face")
    (should (eq '+ai-annotate (get-text-property 0 'face (cdar (+ai--annotate-groups '((1 . "a")) 1)))))))

(ert-deftest +ai--annotate-place/draws-beside-the-lines-from-start ()
  (+ai-test--in-buffer "a\nb\nc\n"
    (let ((+ai-annotate-display 'eol))
      (+ai--annotate-place 2 '((1 . "two") (2 . "three")))
      (ert-info ("explanations should land on the buffer lines counted from START")
        (should (equal '((2 . " ⟵ two") (3 . " ⟵ three")) (+ai-test--annotations))))
      (ert-info ("the buffer should know it carries annotations")
        (should (+ai--annotate-active-p))))))

(ert-deftest +ai--annotate-callback/streams-finished-lines-then-the-rest ()
  (+ai-test--in-buffer "a\nb\n"
    (let* ((+ai-annotate-display 'eol)
           (callback (+ai--annotate-callback (current-buffer) 1 +ai--annotate-run)))
      (funcall callback "1: one\n2: tw" nil)
      (ert-info ("a finished line should show while the next is still arriving")
        (should (equal '((1 . " ⟵ one")) (+ai-test--annotations))))
      (funcall callback "o" nil)
      (funcall callback t nil)
      (ert-info ("the end of the stream should show the last line too")
        (should (equal '((1 . " ⟵ one") (2 . " ⟵ two")) (+ai-test--annotations)))))))

(ert-deftest +ai--annotate-callback/drops-a-reply-to-an-earlier-run ()
  (+ai-test--in-buffer "a\n"
    (let ((callback (+ai--annotate-callback (current-buffer) 1 +ai--annotate-run)))
      (+ai--annotate-clear)
      (funcall callback "1: stale\n" nil)
      (ert-info ("a reply to a run the buffer has since cleared should draw nothing")
        (should (null (+ai-test--annotations)))))))

(ert-deftest +ai--annotate-clear/empties-the-layer-and-clears-on-edit ()
  (+ai-test--in-buffer "a\nb\n"
    (let ((+ai-annotate-display 'eol))
      (+ai--annotate-place 1 '((1 . "x")))
      (ert-info ("an edit should drop the annotations")
        (goto-char (point-max))
        (insert "c")
        (should (null (+ai-test--annotations)))
        (should-not (+ai--annotate-active-p)))
      (+ai--annotate-place 1 '((1 . "x")))
      (ert-info ("clearing should answer t when there was something to clear")
        (should (+ai--annotate-clear))
        (should (null (+ai-test--annotations))))
      (ert-info ("clearing an empty layer should answer nil, so escape passes on")
        (should-not (+ai--annotate-clear))))))
