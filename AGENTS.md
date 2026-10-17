# Working in this repo

This is a Doom Emacs `$DOOMDIR`; README.md describes its layout. Tests run
with `bin/test` (ERT, plain `emacs --batch`) and `bin/compile-check`.

## House rules

- Names follow Doom: `+scope/command` for interactive commands,
  `+scope--helper` for private functions, `+scope-var` for variables.
- Many small named functions over one long one. No comments unless the why
  is non-obvious; then one concise line.
- Every `should` sits inside `ert-info` with a string of the form
  "X should Y". Tests live in `test/<module>-test.el`.
- Run `bin/test` and `bin/compile-check` after each change.
- Do not stage or commit. The jail refuses it; the human reviews and commits.

## The plan gate

While `PLAN.md` exists at the repo root without an `Approved:` line, edit no
file other than `PLAN.md`. The human approves the plan from Emacs; the
`Approved:` line is the record.

## Decisions

Any decision this task forces that `PLAN.md` does not settle, during planning
or later, goes through the grilling interview
(`.agents/skills/grilling/SKILL.md`), never a guess. Append each answer to
`## Decisions` in `PLAN.md` before continuing, in the same `Q → A` form. Say
so when a round changes the test list.

There are no sub-agents here. Look facts up yourself with read, grep and
bash.

## The loop, once the plan is approved

1. Take the next unticked test from `## Tests, in order` in `PLAN.md`.
2. Write it, run `bin/test`, show the failure.
3. Make the smallest change that passes; run `bin/test` and
   `bin/compile-check`.
4. Tick the test in `PLAN.md`. Repeat.
5. Finish with a summary of the diff and how `ACCEPT` is met.
