;;; my/tasks/menu.el -*- lexical-binding: t; -*-

(require 'transient)

(transient-define-prefix +tasks-transient ()
  "Project tasks."
  [["Task"
    ("i" "new idea" +tasks/idea)
    ("I" "sub-idea under task" +tasks/sub-idea)
    ("n" "next: advance task at point" +tasks/next)
    ("s" "start leaf: worktree, Active, interview (C-u: pick its model)" +tasks/start)
    ("c" "continue: worktree, pi session" +tasks/resume)
    ("a" "set ACCEPT" +tasks/accept)
    ("w" "wait: park with a reason" +tasks/wait)]
   ["Work"
    ("r" "review diff" +tasks/review)
    ("R" "AI review vs ACCEPT" +tasks/ai-review)
    ("t" "run tests" +tasks/test)
    ("d" "done: test, merge, remove, DONE (C-u: despite open plan items)" +tasks/done)
    ("x" "abort: drop branch, worktree, back to ready" +tasks/abort)]
   ["Branches"
    ("l" "link row to a task" +tasks/link)
    ("b" "refresh block" +tasks/refresh-branches)]
   ["View"
    ("o" "board" +tasks/board)
    ("O" "board: this project / all" +tasks/toggle-board-scope)
    ("f" "tasks.org" +tasks/open)]])
