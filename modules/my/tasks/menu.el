;;; my/tasks/menu.el -*- lexical-binding: t; -*-

(require 'transient)

(transient-define-prefix +tasks-transient ()
  "Project tasks."
  [["Task"
    ("i" "new idea" +tasks/idea)
    ("s" "start: branch, TODO, Active, pi" +tasks/start)
    ("a" "set ACCEPT" +tasks/accept)]
   ["Work"
    ("r" "review diff" +tasks/review)
    ("R" "AI review vs ACCEPT" +tasks/ai-review)
    ("t" "run tests" +tasks/test)
    ("d" "done: test, merge, delete, DONE" +tasks/done)]
   ["Branches"
    ("l" "link row to a task" +tasks/link)
    ("b" "refresh block" +tasks/refresh-branches)]
   ["View"
    ("o" "board" +tasks/board)
    ("f" "tasks.org" +tasks/open)]])
