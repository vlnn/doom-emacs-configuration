;;; test/ai-summarize-test.el -*- lexical-binding: t; -*-

(defvar gptel-prompt-prefix-alist)

(ert-deftest +ai--summarize-prompt/names-file-and-mode ()
  (ert-info ("prompt should carry the file name and major mode before the source")
    (should (equal "File: app.py (python-mode)\n<source>\nx = 1\n</source>"
                   (+ai--summarize-prompt "app.py" 'python-mode "x = 1")))))

(ert-deftest +ai--summarize-prompt/says-when-outlined ()
  (ert-info ("an outlined prompt should tell the model bodies were dropped")
    (should (string-match-p "bodies omitted"
                            (+ai--summarize-prompt "app.py" 'python-mode "x = 1" t)))))

(ert-deftest +ai--summarize-model/think-wins ()
  (let ((+ai-summarize-model 'custom))
    (ert-info ("C-u should pick the reasoning model over the configured one")
      (should (eq +ai-think-model (+ai--summarize-model t))))
    (ert-info ("without C-u the configured model should win over the fast default")
      (should (eq 'custom (+ai--summarize-model nil))))))

(ert-deftest +ai--summarize-model/fast-by-default ()
  (let ((+ai-summarize-model nil))
    (ert-info ("with nothing set the fast model should be used")
      (should (eq +ai-fast-model (+ai--summarize-model nil))))))

(ert-deftest +ai--summarize-secret-p/matches-ignore-list ()
  (ert-info ("a name on the secrets list should count as secret")
    (should (+ai--summarize-secret-p "secrets.el")))
  (ert-info ("an ordinary name should not")
    (should-not (+ai--summarize-secret-p "app.py"))))

(ert-deftest +ai--summarize-too-long-p/compares-to-max-lines ()
  (let ((+ai-flymake-max-lines 2))
    (ert-info ("more lines than the cap should count as too long")
      (should (+ai--summarize-too-long-p "a\nb\nc")))
    (ert-info ("the cap itself should still fit")
      (should-not (+ai--summarize-too-long-p "a\nb")))))

(ert-deftest +ai--summarize-outline/keeps-top-level-and-signatures ()
  (ert-info ("outline should keep top-level lines, def/class lines at any depth, numbered as in the file")
    (should (equal "1: import os\n2: class A:\n3:     def f(self):\n5:         def g():\n7: x = 1"
                   (+ai--summarize-outline
                    "import os\nclass A:\n    def f(self):\n        y = 2\n        def g():\n            return y\nx = 1")))))

(ert-deftest +ai--summarize-outline/keeps-docstrings-after-signatures ()
  (ert-info ("outline should keep a docstring opening right after a signature")
    (should (equal "1: def f():\n2:     \"\"\"Add one.\"\"\"\n4: async def g():\n5:     'Doc'"
                   (+ai--summarize-outline
                    "def f():\n    \"\"\"Add one.\"\"\"\n    return 1\nasync def g():\n    'Doc'\n    pass")))))

(ert-deftest +ai--summarize-outline/drops-blank-lines ()
  (ert-info ("outline should drop blank lines so the cap counts code only")
    (should (equal "1: a = 1\n4: b = 2" (+ai--summarize-outline "a = 1\n\n\nb = 2\n")))))

(ert-deftest +ai--summarize-source/whole-source-when-short ()
  (let ((+ai-flymake-max-lines 5))
    (ert-info ("a short source should go whole, numbered, not outlined")
      (should (equal '("1: x = 1\n2: y = 2" . nil)
                     (+ai--summarize-source "x = 1\ny = 2"))))))

(ert-deftest +ai--summarize-source/outline-when-long ()
  (let ((+ai-flymake-max-lines 2))
    (ert-info ("a long source should be outlined and say so")
      (should (equal '("1: def f():\n4: x = 1" . t)
                     (+ai--summarize-source "def f():\n    return 1\n    pass\nx = 1"))))))

(ert-deftest +ai--summarize-source/refuses-long-outline ()
  (let ((+ai-flymake-max-lines 1))
    (ert-info ("an outline still over the cap should be refused")
      (should-error (+ai--summarize-source "x = 1\ny = 2") :type 'user-error))))

(ert-deftest +ai--summarize-callback/streams-into-the-buffer ()
  (with-temp-buffer
    (insert "Summarizing...")
    (let ((callback (+ai--summarize-callback (current-buffer))))
      (funcall callback "# Purpose\n" nil)
      (ert-info ("the first chunk should replace the placeholder")
        (should (equal "# Purpose\n" (buffer-string))))
      (funcall callback "Adds." nil)
      (funcall callback t nil)
      (ert-info ("later chunks should append and the end should open the next turn")
        (should (equal "# Purpose\nAdds.\n\n" (buffer-string)))))))

;;; the defun bubble

(ert-deftest +ai--defun-directive/asks-for-behaviour-at-the-count ()
  (let ((directive (+ai--defun-directive 40)))
    (ert-info ("the directive should carry the word budget")
      (should (string-match-p "40 words" directive)))
    (ert-info ("the directive should rule style and bugs out")
      (should (string-match-p "never" directive)))))

(ert-deftest +ai--defun-at-point/the-defun-and-where-it-starts ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun f ()\n  (+ 1 2))\n")
    (goto-char 15)
    (ert-info ("the defun at point should come back with its start")
      (should (equal '("(defun f ()\n  (+ 1 2))" . 1) (+ai--defun-at-point))))))

(ert-deftest +ai--defun-at-point/no-defun-is-a-user-error ()
  (with-temp-buffer
    (ert-info ("with no defun at point the command should say so")
      (should-error (+ai--defun-at-point) :type 'user-error))))

;;; the summary as a chat

(ert-deftest +ai--summarize-append/marks-chunks-as-the-response ()
  (with-temp-buffer
    (insert "Summarizing...")
    (+ai--summarize-append (current-buffer) "Adds.")
    (ert-info ("streamed text should be marked as the model's turn, so follow-ups read as a chat")
      (should (eq 'response (get-text-property 1 'gptel))))))

(ert-deftest +ai--summarize-finish/opens-the-next-turn ()
  (with-temp-buffer
    (insert "Summarizing...")
    (let ((gptel-prompt-prefix-alist '((fundamental-mode . "### "))))
      (+ai--summarize-append (current-buffer) "Adds.")
      (+ai--summarize-finish (current-buffer))
      (ert-info ("the end of the summary should leave a prompt prefix to type after")
        (should (equal "Adds.\n\n### " (buffer-string))))
      (ert-info ("point should sit after the prefix, ready to type")
        (should (= (point) (point-max))))
      (ert-info ("the buffer should be done")
        (should (eq 'done +ai--summarize-state))))))

;;; entry points that jump

(ert-deftest +ai--summarize-purpose/the-sentence-under-the-heading ()
  (ert-info ("purpose should be the text under # Purpose, trimmed")
    (should (equal "Adds numbers."
                   (+ai--summarize-purpose "# Purpose\n\nAdds numbers.\n\n# Entry points\nmain (line 3)"))))
  (ert-info ("no heading should give nil")
    (should-not (+ai--summarize-purpose "nothing here"))))

(ert-deftest +ai--summarize-link-lines/buttons-every-line-reference ()
  (with-temp-buffer
    (insert "main (line 3), helper (line 12)")
    (+ai--summarize-link-lines (current-buffer))
    (ert-info ("each (line N) should become a button carrying N")
      (should (equal '(3 12)
                     (mapcar (lambda (b) (button-get b 'line))
                             (seq-filter #'identity (list (button-at 7) (button-at 25)))))))))

(ert-deftest +ai--summarize-goto-line/shows-the-source-at-the-line ()
  (let ((source (generate-new-buffer "src")))
    (unwind-protect
        (with-current-buffer source
          (insert "a\nb\nc\n")
          (+ai--summarize-goto-line source 3)
          (ert-info ("point in the source should be on the line asked")
            (should (= 3 (line-number-at-pos)))))
      (kill-buffer source))))

;;; the cache and the header-line

(ert-deftest +ai--summarize-fresh/remembers-until-the-buffer-changes ()
  (with-temp-buffer
    (insert "x = 1")
    (ert-info ("before any summary nothing should be fresh")
      (should-not (+ai--summarize-fresh)))
    (+ai--summarize-remember (current-buffer) "# Purpose\nSets x.")
    (ert-info ("right after remembering the summary should be fresh")
      (should (equal "# Purpose\nSets x." (+ai--summarize-fresh))))
    (ert-info ("the header should show the purpose while fresh")
      (should (equal " Sets x." (+ai--summarize-header))))
    (insert "\ny = 2")
    (ert-info ("an edit should make the summary stale")
      (should-not (+ai--summarize-fresh)))
    (ert-info ("the header should go with it")
      (should-not (+ai--summarize-header)))))

(ert-deftest +ai--summarize-finish/remembers-into-the-source ()
  (let ((source (generate-new-buffer "src")))
    (unwind-protect
        (with-temp-buffer
          (setq +ai--summarize-source source)
          (insert "Summarizing...")
          (+ai--summarize-append (current-buffer) "# Purpose\nAdds.")
          (+ai--summarize-finish (current-buffer))
          (ert-info ("finishing should leave the summary cached in the source buffer")
            (should (equal "# Purpose\nAdds." (with-current-buffer source (+ai--summarize-fresh))))))
      (kill-buffer source))))
