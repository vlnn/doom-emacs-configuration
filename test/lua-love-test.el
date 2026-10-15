;;; test/lua-love-test.el -*- lexical-binding: t; -*-

(defun +lua-love-test--project (files)
  "Create a temp project holding FILES, an alist of (NAME . CONTENT); return its root."
  (let ((root (file-name-as-directory (make-temp-file "love-project" t))))
    (pcase-dolist (`(,name . ,content) files)
      (with-temp-file (expand-file-name name root) (insert content)))
    root))

(ert-deftest +lua--love-hook-line/loads-repl-only-from-emacs ()
  (ert-info ("the hook line should load the REPL only when Emacs set the env var")
    (should (equal "if os.getenv(\"EMACS_LOVE_REPL\") then dofile(os.getenv(\"EMACS_LOVE_REPL\")) end"
                   +lua-love-hook-line))))

(ert-deftest +lua--love-hooked-p/detects-the-hook ()
  (pcase-dolist (`(,files ,expected ,why)
                 `(((("conf.lua" . ,+lua-love-hook-line) ("main.lua" . "function love.load() end")) t "a hook in conf.lua")
                   ((("main.lua" . ,(concat +lua-love-hook-line "\nfunction love.load() end")))     t "a hook in main.lua")
                   ((("main.lua" . "function love.load() end"))                                   nil "a game without the hook")
                   ((("init.lua" . "return {}"))                                                  nil "a plain Lua project")))
    (let ((root (+lua-love-test--project files)))
      (unwind-protect
          (ert-info ((format "hook detection should recognise %s" why))
            (should (eq expected (+lua--love-hooked-p root))))
        (delete-directory root t)))))

(ert-deftest +lua--with-love-hook/prepends-the-hook-once ()
  (pcase-dolist (`(,conf ,expected ,why)
                 `(("function love.conf(t) end\n"
                    ,(concat +lua-love-hook-line "\n\nfunction love.conf(t) end\n") "an existing conf.lua")
                   (""
                    ,(concat +lua-love-hook-line "\n") "a new conf.lua")
                   (,(concat +lua-love-hook-line "\nfunction love.conf(t) end\n")
                    ,(concat +lua-love-hook-line "\nfunction love.conf(t) end\n") "a conf.lua that already has the hook")))
    (ert-info ((format "adding the hook should handle %s" why))
      (should (equal expected (+lua--with-love-hook conf))))))

(ert-deftest +lua--love-environment/points-game-at-repl-and-interrupt-file ()
  (ert-info ("the game environment should carry the REPL script and the interrupt file")
    (should (equal (list (concat "EMACS_LOVE_REPL=" +lua-love-repl-file)
                         "EMACS_LOVE_REPL_INTERRUPT=/tmp/int")
                   (+lua--love-environment "/tmp/int")))))

;;; Integration: a real LÖVE game, headless under xvfb.

(defun +lua-love-test--command ()
  (when (executable-find "love")
    (if (or (getenv "DISPLAY") (not (executable-find "xvfb-run")))
        '("love")
      '("xvfb-run" "-a" "love"))))

(defconst +lua-love-test--main
  "loaded = false
function love.load()
  debug = false
  loaded = true
  print('hello from love.load')
end
function speed() return 1 end")

(defmacro +lua-love-test--with-game (&rest body)
  "Run BODY with `proc' bound to the REPL of a fresh hooked LÖVE game."
  (declare (indent 0))
  `(let* ((root (+lua-love-test--project `(("conf.lua" . ,+lua-love-hook-line)
                                          ("main.lua" . ,+lua-love-test--main))))
          (+lua-love-command (+lua-love-test--command))
          (process-environment (append '("SDL_AUDIODRIVER=dummy" "ALSOFT_DRIVERS=null") process-environment))
          (proc (+lua--start-love-repl root)))
     (unwind-protect
         (progn ,@body)
       (set-process-query-on-exit-flag proc nil)
       (kill-buffer (process-buffer proc))
       (delete-directory root t))))

(ert-deftest +lua-love/evaluates-inside-the-running-game ()
  (skip-unless (+lua-love-test--command))
  (+lua-love-test--with-game
    (ert-info ("the love table should exist in the REPL")
      (should (string-prefix-p "11, " (plist-get (+lua-test--eval proc "love.getVersion()") :value))))
    (ert-info ("love.load should have run before the REPL answers")
      (should (equal "true" (plist-get (+lua-test--eval proc "loaded") :value))))
    (ert-info ("the game's printed output should reach the REPL buffer")
      (should (string-match-p "hello from love.load" (+lua-test--repl-text proc))))))

(ert-deftest +lua-love/redefinitions-reach-the-game-loop ()
  (skip-unless (+lua-love-test--command))
  (+lua-love-test--with-game
    (+lua-test--eval proc "function love.update() frames = (frames or 0) + 1 end")
    (sleep-for 0.5)
    (ert-info ("a love.update redefined from Emacs should be called by the running game")
      (should (equal "true" (plist-get (+lua-test--eval proc "frames > 0") :value))))))

(ert-deftest +lua-love/answers-typed-lines-and-continuations ()
  (skip-unless (+lua-love-test--command))
  (+lua-love-test--with-game
    (ert-info ("a line typed in the game REPL should be answered")
      (should (string-suffix-p "2\n> " (+lua-test--type proc "speed() + 1\n" "2\n> "))))
    (ert-info ("continuation lines should be gathered across frames")
      (should (string-suffix-p "7\n> " (+lua-test--type proc "function h()\n  return 7\nend\nh()\n" "7\n> "))))))

(ert-deftest +lua-love/interrupts-a-runaway-eval ()
  (skip-unless (+lua-love-test--command))
  (+lua-love-test--with-game
    (with-timeout (0.5 nil)
      (+lua-test--eval proc "jit.off() while true do end"))
    (ert-info ("the game should survive an interrupted eval")
      (should (process-live-p proc)))
    (ert-info ("the game REPL should answer after the interrupt")
      (should (equal "2" (plist-get (with-timeout (5 '(:value timed-out)) (+lua-test--eval proc "1 + 1"))
                                    :value))))))
