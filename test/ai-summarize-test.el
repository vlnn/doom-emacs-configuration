;;; test/ai-summarize-test.el -*- lexical-binding: t; -*-

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
  (ert-info ("outline should keep top-level lines, def/class lines at any depth and nothing else")
    (should (equal "import os\nclass A:\n    def f(self):\n        def g():\nx = 1"
                   (+ai--summarize-outline
                    "import os\nclass A:\n    def f(self):\n        y = 2\n        def g():\n            return y\nx = 1")))))

(ert-deftest +ai--summarize-outline/keeps-docstrings-after-signatures ()
  (ert-info ("outline should keep a docstring opening right after a signature")
    (should (equal "def f():\n    \"\"\"Add one.\"\"\"\nasync def g():\n    'Doc'"
                   (+ai--summarize-outline
                    "def f():\n    \"\"\"Add one.\"\"\"\n    return 1\nasync def g():\n    'Doc'\n    pass")))))

(ert-deftest +ai--summarize-outline/drops-blank-lines ()
  (ert-info ("outline should drop blank lines so the cap counts code only")
    (should (equal "a = 1\nb = 2" (+ai--summarize-outline "a = 1\n\n\nb = 2\n")))))

(ert-deftest +ai--summarize-source/whole-source-when-short ()
  (let ((+ai-flymake-max-lines 5))
    (ert-info ("a short source should go as it is, not outlined")
      (should (equal '("x = 1\ny = 2" . nil)
                     (+ai--summarize-source "x = 1\ny = 2"))))))

(ert-deftest +ai--summarize-source/outline-when-long ()
  (let ((+ai-flymake-max-lines 2))
    (ert-info ("a long source should be outlined and say so")
      (should (equal '("def f():\nx = 1" . t)
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
      (ert-info ("later chunks should append and the end should change nothing")
        (should (equal "# Purpose\nAdds." (buffer-string)))))))
