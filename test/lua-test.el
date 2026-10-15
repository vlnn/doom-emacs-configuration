;;; test/lua-test.el -*- lexical-binding: t; -*-

(ert-deftest +lua--string-literal/escapes-to-single-line ()
  (pcase-dolist (`(,input ,expected ,why)
                 '(("x = 1"     "'x = 1'"      "plain code")
                   ("it's"      "'it\\'s'"     "single quotes")
                   ("say \"hi\"" "'say \\\"hi\\\"'" "double quotes")
                   ("a\\b"      "'a\\\\b'"     "backslashes")
                   ("a\nb"      "'a\\nb'"      "newlines")
                   ("a\rb"      "'a\\rb'"      "carriage returns")
                   ("a\tb"      "'a\\tb'"      "tabs")
                   ("\3" "'\\003'"           "control characters that a tty would act on")
                   ("\0337"     "'\\0277'"     "control characters followed by digits")))
    (ert-info ((format "string literal should escape %s" why))
      (should (equal expected (+lua--string-literal input))))))

(ert-deftest +lua--eval-command/points-evaluator-at-code-file ()
  (ert-info ("eval command should pass the code file, chunk name and first line to the evaluator")
    (should (equal "__emacs_eval_file('/tmp/c.lua', '/p/a.lua', 7)"
                   (+lua--eval-command "/tmp/c.lua" "/p/a.lua" 7)))))

(ert-deftest +lua--unframed-reply/keeps-repl-message-without-prompt ()
  (let ((reply (+lua--unframed-reply "stdin:1: attempt to call a nil value\nstack traceback:\n\t[C]: in ?\n> ")))
    (ert-info ("an unframed reply should be an error")
      (should (eq 'err (plist-get reply :status))))
    (ert-info ("an unframed reply should keep the REPL's message and drop the prompt line")
      (should (equal "stdin:1: attempt to call a nil value\nstack traceback:\n\t[C]: in ?"
                     (plist-get reply :value))))))

(ert-deftest +lua--load-evaluator-command/dofiles-path-and-acknowledges ()
  (pcase-dolist (`(,interrupt-file ,expected ,why)
                 '(("/tmp/int" "dofile('/x/evaluator.lua').set_interrupt_file('/tmp/int') io.write('\\003\\n')"
                    "the REPL's interrupt file")
                   (nil        "dofile('/x/evaluator.lua').set_interrupt_file(nil) io.write('\\003\\n')"
                    "no interrupt file for a stock lua -i")))
    (ert-info ((format "load command should dofile the evaluator, hand it %s and end with the reply terminator" why))
      (should (equal expected (+lua--load-evaluator-command "/x/evaluator.lua" interrupt-file))))))

(ert-deftest +lua--parse-reply/splits-output-from-value ()
  (pcase-dolist (`(,raw ,expected ,why)
                 '(("\2ok\0373\3\n"
                    (:stdout "" :status ok :value "3")
                    "a bare value")
                   ("hello\nworld\n\2ok\037nil\3\n"
                    (:stdout "hello\nworld\n" :status ok :value "nil")
                    "printed output before the value")
                   ("\2err\037a.lua:3: boom\3\n"
                    (:stdout "" :status err :value "a.lua:3: boom")
                    "an error status")
                   ("\2ok\037{\n  1\n}\3\n"
                    (:stdout "" :status ok :value "{\n  1\n}")
                    "a multi-line value")))
    (ert-info ((format "reply parser should handle %s" why))
      (should (equal expected (+lua--parse-reply raw))))))

(ert-deftest +lua--parse-reply/nil-without-markers ()
  (ert-info ("reply parser should return nil when the evaluator did not answer")
    (should-not (+lua--parse-reply "stdin:1: attempt to call a nil value\n> "))))

(ert-deftest +lua--reply-text/marks-errors ()
  (pcase-dolist (`(,reply ,expected ,why)
                 '(((:status ok :value "42")        "42"            "values as is")
                   ((:status err :value "a.lua:1: x") "error: a.lua:1: x" "errors with a prefix")))
    (ert-info ((format "reply text should show %s" why))
      (should (equal expected (+lua--reply-text reply))))))

(ert-deftest +lua--as-comment/prefixes-every-line ()
  (pcase-dolist (`(,value ,expected ,why)
                 '(("3"              "-- => 3"                 "a one-line value")
                   ("{\n  1,\n  2\n}" "-- => {\n--   1,\n--   2\n-- }" "a multi-line value")))
    (ert-info ((format "result comment should render %s" why))
      (should (equal expected (+lua--as-comment value))))))

(ert-deftest +lua--lua-pattern-quote/escapes-magic-characters ()
  (pcase-dolist (`(,input ,expected)
                 '(("adds numbers"   "adds numbers")
                   ("handles 100%"   "handles 100%%")
                   ("f(x) -> [y]?"   "f%(x%) %-> %[y%]%?")
                   ("a.b*c+d^e$"     "a%.b%*c%+d%^e%$")))
    (ert-info ((format "%S should become the literal Lua pattern %S" input expected))
      (should (equal expected (+lua--lua-pattern-quote input))))))

(ert-deftest +lua--nearest-test-name/finds-preceding-it-block ()
  (pcase-dolist (`(,code ,marker ,expected ,why)
                 '(("describe('calc', function()\n  it('adds two', function()\n    assert(true)|\n  end)\nend)"
                    "|" "adds two" "single-quoted names")
                   ("it(\"subtracts\", function()\n  |assert(true)\nend)"
                    "|" "subtracts" "double-quoted names")
                   ("it('first', function() end)\nit('second', function()\n  |x()\nend)"
                    "|" "second" "the closest preceding test")
                   ("local edit = 1\n|"
                    "|" nil "no test at all")))
    (with-temp-buffer
      (insert code)
      (goto-char (point-min))
      (search-forward marker)
      (delete-char -1)
      (ert-info ((format "nearest test lookup should handle %s" why))
        (should (equal expected (+lua--nearest-test-name)))))))

(ert-deftest +lua--spec-file-for/locates-busted-specs ()
  (pcase-dolist (`(,file ,existing ,expected ,why)
                 '(("/p/spec/calc_spec.lua" ()                       "/p/spec/calc_spec.lua" "a spec file itself")
                   ("/p/calc.lua"           ("/p/calc_spec.lua")     "/p/calc_spec.lua"      "a sibling spec")
                   ("/p/src/calc.lua"       ("/p/spec/calc_spec.lua") "/p/spec/calc_spec.lua" "a spec mirrored under spec/ without src/")
                   ("/p/lua/m/calc.lua"     ("/p/spec/m/calc_spec.lua") "/p/spec/m/calc_spec.lua" "a nested spec without lua/")
                   ("/p/calc.lua"           ()                       nil                     "a file without specs")))
    (ert-info ((format "spec lookup should find %s" why))
      (should (equal expected
                     (+lua--spec-file-for file "/p/" (lambda (f) (member f existing))))))))

(ert-deftest +lua--busted-command/builds-shell-command ()
  (let ((+lua-busted-program "busted"))
    (pcase-dolist (`(,args ,expected ,why)
                   '((()                                  "busted"                          "the whole project")
                     (("spec/a_spec.lua")                 "busted spec/a_spec.lua"          "one file")
                     (("--filter=adds two" "spec/a_spec.lua") "busted --filter\\=adds\\ two spec/a_spec.lua" "a filtered test")))
      (ert-info ((format "busted command should run %s" why))
        (should (equal expected (apply #'+lua--busted-command args)))))))

;;; Integration: a real `lua -i' behind comint, the way lua-mode runs it.

(defun +lua-test--interpreter ()
  (seq-some #'executable-find (delq nil (list (getenv "LUA") "lua" "lua5.4" "lua5.3" "luajit"))))

(defun +lua-test--at-prompt-p ()
  (save-excursion
    (goto-char (point-max))
    (forward-line 0)
    (looking-at-p comint-prompt-regexp)))

(defun +lua-test--wait-for-prompt (proc)
  (with-current-buffer (process-buffer proc)
    (with-timeout (10 (error "Lua REPL never showed a prompt"))
      (while (not (+lua-test--at-prompt-p))
        (accept-process-output proc 0.05)))))

(defmacro +lua-test--with-repl (&rest body)
  "Run BODY with `proc' bound to a fresh Lua REPL started the way lua-mode starts ours.
Like `lua-start-process', an init line is sent without waiting for its prompt."
  (declare (indent 0))
  `(let* ((interrupt-file (make-temp-name (expand-file-name "lua-test-interrupt" temporary-file-directory)))
          (buffer (let ((process-connection-type nil))
                    (apply #'make-comint-in-buffer "lua-test" nil (+lua-test--interpreter) nil
                           (+lua--repl-switches interrupt-file))))
          (proc (get-buffer-process buffer)))
     (unwind-protect
         (progn
           (with-current-buffer buffer
             (setq-local comint-prompt-regexp "[^\n]*\\(>[\t ]+\\)+$"))
           (+lua-test--wait-for-prompt proc)
           (process-send-string proc "init_done = true\n")
           (+lua--ensure-evaluator proc)
           ,@body)
       (set-process-query-on-exit-flag proc nil)
       (kill-buffer buffer)
       (ignore-errors (delete-file interrupt-file)))))

(defun +lua-test--repl-text (proc)
  (with-current-buffer (process-buffer proc)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun +lua-test--type (proc input expected-tail)
  "Send INPUT as if typed in the REPL and wait until the buffer ends with EXPECTED-TAIL."
  (process-send-string proc input)
  (with-timeout (5 nil)
    (while (not (string-suffix-p expected-tail (+lua-test--repl-text proc)))
      (accept-process-output proc 0.05)))
  (+lua-test--repl-text proc))

(ert-deftest +lua--repl-switches/runs-evaluator-as-the-program ()
  (ert-info ("the REPL should run evaluator.lua as a script with the interrupt file, not lua -i")
    (should (equal (list +lua-evaluator-file "/tmp/int")
                   (+lua--repl-switches "/tmp/int")))))

(ert-deftest +lua--interrupt-file/read-from-the-process-command ()
  (pcase-dolist (`(,command ,expected ,why)
                 `(((,"lua" ,+lua-evaluator-file "/tmp/int") "/tmp/int" "our REPL")
                   (("lua" "-i")                             nil        "a stock lua -i REPL")))
    (ert-info ((format "interrupt file lookup should handle %s" why))
      (should (equal expected (+lua--interrupt-file-in command))))))

(ert-deftest +lua-repl/answers-typed-lines-like-the-lua-repl ()
  (skip-unless (+lua-test--interpreter))
  (+lua-test--with-repl
    (pcase-dolist (`(,input ,tail ,why)
                   '(("y = 40\n"                            "> "           "a statement")
                     ("y + 2\n"                             "42\n> "       "an expression")
                     ("{1, 'a'}\n"                          "{ 1, \"a\" }\n> " "a table")
                     ("function g()\n  return 3\nend\ng()\n" "3\n> "        "continuation lines")
                     ("error('nope')\n"                     "stdin:1: nope\n> " "an error")))
      (ert-info ((format "typing %s should behave like the Lua REPL" why))
        (should (string-suffix-p tail (+lua-test--type proc input tail)))))))

(ert-deftest +lua-repl/first-eval-is-not-shifted-by-init-prompt ()
  (skip-unless (+lua-test--interpreter))
  (+lua-test--with-repl
    (ert-info ("the first eval after start should get its own value, not a stray init prompt")
      (should (equal '("1" "2")
                     (list (plist-get (+lua-test--eval proc "1") :value)
                           (plist-get (+lua-test--eval proc "2") :value)))))
    (ert-info ("the stray init prompt should not leak into printed output")
      (should (equal "" (plist-get (+lua-test--eval proc "3") :stdout))))))

(defun +lua-test--eval (proc code &optional line)
  (+lua--eval-in-process proc code "/p/a.lua" (or line 1)))

(ert-deftest +lua-repl/evaluates-like-the-lua-repl ()
  (skip-unless (+lua-test--interpreter))
  (+lua-test--with-repl
    (pcase-dolist (`(,code ,expected ,why)
                   '(("1 + 2"                     "3"                     "an expression")
                     ("1 + 2;"                    "3"                     "an expression with a trailing semicolon")
                     ("1 + 2 -- sum"              "3"                     "an expression with a trailing comment")
                     ("1, 'two', nil"             "1, \"two\", nil"       "multiple values")
                     ("x = 41"                    "nil"                   "a statement")
                     ("x + 1"                     "42"                    "state kept between evals")
                     ("local function f() return 7 end\nreturn f()" "7"   "a multi-line chunk")
                     ("'a\\nb'"                   "\"a\\nb\""             "escaped strings")
                     ("{}"                        "{}"                    "an empty table")
                     ("{1, 2, 3}"                 "{ 1, 2, 3 }"           "a sequence")
                     ("{b = 2, a = 1}"            "{ a = 1, b = 2 }"      "a record with sorted keys")
                     ("{[2] = 'x', ['k-1'] = {}}" "{ [2] = \"x\", [\"k-1\"] = {} }" "keys that need brackets")))
      (ert-info ((format "evaluator should render %s" why))
        (should (equal `(ok ,expected)
                       (let ((reply (+lua-test--eval proc code)))
                         (list (plist-get reply :status) (plist-get reply :value)))))))))

(ert-deftest +lua-repl/evaluates-chunks-longer-than-a-tty-line ()
  (skip-unless (+lua-test--interpreter))
  (+lua-test--with-repl
    (let* ((items (mapconcat #'number-to-string (number-sequence 1 2000) ",\n  "))
           (code (format "local function size()\n  local t = {\n  %s\n  }\n  return #t\nend\nreturn size()" items)))
      (ert-info ("a chunk bigger than the pty's input line limit should still evaluate")
        (should (equal "2000"
                       (plist-get (with-timeout (5 '(:value timed-out)) (+lua-test--eval proc code))
                                  :value)))))))

(ert-deftest +lua-repl/keeps-command-lines-under-the-macos-tty-limit ()
  (skip-unless (+lua-test--interpreter))
  (+lua-test--with-repl
    (let ((sent nil)
          (code (concat "return #'" (make-string 5000 ?x) "'")))
      (advice-add 'process-send-string :before
                  (lambda (_proc string) (push string sent))
                  '((name . +lua-test--spy)))
      (unwind-protect (+lua-test--eval proc code)
        (advice-remove 'process-send-string '+lua-test--spy))
      (ert-info ("every line sent to the REPL should stay under macOS's 1024-byte canonical limit")
        (should (seq-every-p (lambda (s) (< (string-bytes s) 1024)) sent))))))

(ert-deftest +lua-repl/cleans-up-code-files ()
  (skip-unless (+lua-test--interpreter))
  (let ((temporary-file-directory (file-name-as-directory (make-temp-file "lua-eval-test" t))))
    (unwind-protect
        (+lua-test--with-repl
          (+lua-test--eval proc "1")
          (ert-info ("evaluation should not leave code files behind")
            (should-not (directory-files temporary-file-directory nil "\\.lua\\'"))))
      (delete-directory temporary-file-directory t))))

(ert-deftest +lua-repl/survives-code-that-clobbers-standard-globals ()
  (skip-unless (+lua-test--interpreter))
  (+lua-test--with-repl
    (+lua-test--eval proc "debug, print, io, os, string, table, tostring, pcall, select, error = false")
    (ert-info ("evaluating after the standard globals were overwritten should still answer")
      (should (equal "2" (plist-get (with-timeout (3 '(:value timed-out)) (+lua-test--eval proc "1 + 1"))
                                    :value))))
    (ert-info ("typed lines should still be answered after the standard globals were overwritten")
      (should (string-suffix-p "3\n> " (+lua-test--type proc "1 + 2\n" "3\n> "))))))

(ert-deftest +lua-repl/marks-cycles ()
  (skip-unless (+lua-test--interpreter))
  (+lua-test--with-repl
    (ert-info ("evaluator should not loop forever on a self-referencing table")
      (should (equal "{ self = <cycle> }"
                     (plist-get (+lua-test--eval proc "local t = {}; t.self = t; return t") :value))))))

(ert-deftest +lua-repl/wraps-long-tables ()
  (skip-unless (+lua-test--interpreter))
  (+lua-test--with-repl
    (ert-info ("evaluator should break tables wider than one line into indented lines")
      (should (equal "{\n  alpha = \"aaaaaaaaaaaaaaaaaaaa\",\n  beta = \"bbbbbbbbbbbbbbbbbbbb\",\n  gamma = { 1, 2 }\n}"
                     (plist-get (+lua-test--eval proc "{alpha = ('a'):rep(20), beta = ('b'):rep(20), gamma = {1, 2}}")
                                :value))))))

(ert-deftest +lua-repl/captures-printed-output-separately ()
  (skip-unless (+lua-test--interpreter))
  (+lua-test--with-repl
    (let ((reply (+lua-test--eval proc "print('hi'); return 5")))
      (ert-info ("evaluator should keep printed output out of the value")
        (should (equal "5" (plist-get reply :value))))
      (ert-info ("evaluator should hand printed output back as stdout")
        (should (equal "hi\n" (plist-get reply :stdout))))
      (+lua--echo-to-repl proc reply)
      (ert-info ("echoed output should land above the REPL prompt")
        (should (string-suffix-p "hi\n> " (with-current-buffer (process-buffer proc)
                                            (buffer-substring-no-properties (point-min) (point-max)))))))))

(ert-deftest +lua-repl/reports-errors-at-source-lines ()
  (skip-unless (+lua-test--interpreter))
  (+lua-test--with-repl
    (pcase-dolist (`(,code ,line ,expected ,why)
                   '(("error('boom')"    12 "/p/a.lua:12: boom"  "runtime errors")
                     ("\nlocal = "        3 "/p/a.lua:4:"        "syntax errors on later lines")))
      (let ((reply (+lua-test--eval proc code line)))
        (ert-info ((format "evaluator should flag %s as errors" why))
          (should (eq 'err (plist-get reply :status))))
        (ert-info ((format "evaluator should point %s at the buffer line" why))
          (should (string-prefix-p expected (plist-get reply :value))))))))

(ert-deftest +lua-repl/recovers-after-an-interrupted-eval ()
  (skip-unless (+lua-test--interpreter))
  (+lua-test--with-repl
    (ert-info ("quitting a runaway eval should leave the REPL usable")
      (with-timeout (0.5 nil)
        (+lua-test--eval proc "if jit then jit.off() end while true do end"))
      (should (equal "2" (plist-get (+lua-test--eval proc "1 + 1") :value))))))

(ert-deftest +lua-repl/interrupts-repeatedly-without-killing-lua ()
  (skip-unless (+lua-test--interpreter))
  (+lua-test--with-repl
    (dotimes (attempt 3)
      (with-timeout (0.5 nil)
        (+lua-test--eval proc "if jit then jit.off() end while true do end"))
      (ert-info ((format "Lua should survive interrupt number %d" (1+ attempt)))
        (should (process-live-p proc)))
      (ert-info ((format "the REPL should answer after interrupt number %d" (1+ attempt)))
        (should (equal "2" (plist-get (with-timeout (3 '(:value timed-out)) (+lua-test--eval proc "1 + 1"))
                                      :value)))))))

(ert-deftest +lua-repl/interrupt-at-idle-keeps-lua-alive ()
  (skip-unless (+lua-test--interpreter))
  (+lua-test--with-repl
    (+lua--request-interrupt proc)
    (accept-process-output proc 0.2)
    (ert-info ("interrupting an idle REPL should not kill Lua")
      (should (process-live-p proc)))
    (ert-info ("a stale interrupt should not cancel the next eval")
      (should (equal "1" (plist-get (with-timeout (3 '(:value timed-out)) (+lua-test--eval proc "1"))
                                    :value))))))

(ert-deftest +lua-repl/reports-a-repl-stuck-in-c-code ()
  (skip-unless (and (+lua-test--interpreter) (executable-find "sleep")))
  (+lua-test--with-repl
    (with-timeout (0.5 nil)
      (+lua-test--eval proc "os.execute('sleep 3')"))
    (let ((reply (with-timeout (3 '(:status timed-out)) (+lua-test--eval proc "1"))))
      (ert-info ("evaluating while Lua is blocked in C should fail fast")
        (should (eq 'err (plist-get reply :status))))
      (ert-info ("the stuck-REPL error should suggest a restart")
        (should (string-match-p "busy.*restart" (plist-get reply :value)))))
    (sleep-for 3)
    (accept-process-output proc 0.2)
    (ert-info ("the REPL should be usable again once the blocking call returns")
      (should (equal "1" (plist-get (with-timeout (3 '(:value timed-out)) (+lua-test--eval proc "1"))
                                    :value))))))

(ert-deftest +lua-repl/survives-an-unloaded-evaluator ()
  (skip-unless (+lua-test--interpreter))
  (+lua-test--with-repl
    (+lua--eval-in-process proc "__emacs_eval_file = nil" "/p/a.lua" 1)
    (let ((reply (with-timeout (3 '(:status timed-out)) (+lua-test--eval proc "1"))))
      (ert-info ("evaluating after the evaluator vanished should report the REPL error instead of hanging")
        (should (eq 'err (plist-get reply :status))))
      (ert-info ("the REPL error should name the missing evaluator")
        (should (string-match-p "__emacs_eval_file" (plist-get reply :value)))))
    (ert-info ("the next eval should reload the evaluator and work again")
      (should (equal "1" (plist-get (with-timeout (3 '(:value timed-out)) (+lua-test--eval proc "1"))
                                    :value))))))
