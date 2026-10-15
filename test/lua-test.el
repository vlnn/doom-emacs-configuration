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

(ert-deftest +lua--eval-command/calls-evaluator-with-location ()
  (ert-info ("eval command should pass code, chunk name and first line to the evaluator or its fallback")
    (should (equal "(__emacs_eval or __emacs_eval_missing)('return 1', '/p/a.lua', 7)"
                   (+lua--eval-command "return 1" "/p/a.lua" 7)))))

(ert-deftest +lua--load-evaluator-command/dofiles-path-and-acknowledges ()
  (ert-info ("load command should dofile the evaluator and end with the reply terminator")
    (should (equal "dofile('/x/evaluator.lua') io.write('\\003\\n')"
                   (+lua--load-evaluator-command "/x/evaluator.lua")))))

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
  "Run BODY with `proc' bound to a fresh Lua REPL that has the evaluator loaded.
Like `lua-start-process', an init line is sent without waiting for its prompt."
  (declare (indent 0))
  `(let* ((buffer (make-comint-in-buffer "lua-test" nil (+lua-test--interpreter) nil "-i"))
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
       (kill-buffer buffer))))

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

(ert-deftest +lua-repl/reports-a-repl-that-ignored-the-interrupt ()
  (skip-unless (+lua-test--interpreter))
  (+lua-test--with-repl
    (with-timeout (0.5 nil)
      (+lua-test--eval proc "while true do pcall(function() while true do end end) end"))
    (let ((reply (with-timeout (3 '(:status timed-out)) (+lua-test--eval proc "1"))))
      (ert-info ("evaluating while the REPL is stuck should fail fast")
        (should (eq 'err (plist-get reply :status))))
      (ert-info ("the stuck-REPL error should suggest a restart")
        (should (string-match-p "busy.*restart" (plist-get reply :value)))))))

(ert-deftest +lua-repl/survives-an-unloaded-evaluator ()
  (skip-unless (+lua-test--interpreter))
  (+lua-test--with-repl
    (+lua--eval-in-process proc "__emacs_eval = nil" "/p/a.lua" 1)
    (let ((reply (with-timeout (3 '(:status timed-out)) (+lua-test--eval proc "1"))))
      (ert-info ("evaluating after the evaluator vanished should report that instead of hanging")
        (should (eq 'err (plist-get reply :status))))
      (ert-info ("the missing-evaluator error should say what is wrong")
        (should (string-match-p "not loaded" (plist-get reply :value)))))))
