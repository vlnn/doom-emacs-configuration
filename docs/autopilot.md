# Autopilot: the stretch after approval

`docs/planning-flow.md` is the design of the plan gate: nothing is written
until you approve `PLAN.md`. This is the design of what follows it: from
`Approved:` to REVIEW, no key of yours is needed. Decided 2026-10-19 in a
grilling session; the `Q → A` lines are at the end.

## What stays yours

Four keys and one edit. `s` chooses what gets worked on, `y` what gets
built, the commit is yours after reading the diff, `d` lands it on `main`.
The edit is the model: `m` flips `:MODEL:` between `local` and `claude`.
Everything else between approval and REVIEW fires on events.

The Board has to be true for that, so PLAN, RED, GREEN, REVIEW and DONE are
set only by the commands that mean them: an `org-blocker-hook`, buffer-local
in `tasks.org` like the parent block, refuses those five keywords unless
`+tasks--moving` is bound, and the commands bind it. IDEA, TODO, WAIT and
DROPPED stay yours to type.

## How Emacs learns pi is done

pi has no way to tell Emacs anything today; `PLAN.md` is the one file both
read. Two signals, either one enough:

- A pi extension, `modules/my/ai/settled.ts`, passed by `--extension` through
  `pilish-extra-args` like the model flags. On `agent_settled` (the event
  after every automatic continuation is spent; `agent_end` fires per run and
  a queued message can follow it) it writes `.pi-settled` at the worktree
  root. The file is excluded with `PLAN.md`; a `file-notify` watch on the
  worktree sees it. It lives in the config, not the project: nothing to copy,
  nothing a project can swap, and `+tasks-guarded-files` already covers the
  directory.
- `PLAN.md` itself, watched the same way: the tick count reaching the total
  counts as settled when the extension is absent.

Settled means one of two things, and `PLAN.md` tells them apart. pi must end
with a `## Done` section — the diff summary and how `ACCEPT` is met that
`AGENTS.md` already asks for, under a fixed heading. Settled with `## Done`:
run the tests. Settled without it: pi has a question (the grilling rule says
it asks rather than guesses), so the task goes to WAIT with `:REASON: pi
asks: <its last line>`; `n` on it resumes the session as now.

## The loop

```
y ──▶ RED ──settled+Done──▶ t ──GREEN──▶ R ──▶ REVIEW ──n──▶ diff, commit ──n──▶ d
       │                     │
       │                     └──RED, TRY < TRIES_TOTAL──▶ failure sent back, TRY+1 ──▶ RED
       │                     └──RED, TRY = TRIES_TOTAL──▶ WAIT "out of tries"
       ├──settled, no Done──▶ WAIT "pi asks: …"
       └──no progress N min─▶ WAIT "no progress for N min"
```

- **Tests.** `t` as today: the worktree's command, in the jail, verdict on
  `compilation-finish-functions`. The compilation buffer is not shown.
- **RED after Done.** The failure goes back once per try: the tail of the
  compilation as "Tests fail after Done:", and `## Done` is removed so the
  next settle is read fresh. `:TRY:` counts up on the task, visible on the
  Board; `TRIES_TOTAL` caps it, `#+PROPERTY: TRIES_TOTAL_ALL 4` in the
  skeleton, overridable per task. The cap reached: WAIT, reason "RED after
  4 tries". `y` echoes the cap when it starts the stretch.
- **Stall.** A timer from the last progress (a tick in `PLAN.md`, a marker,
  a state change): 20 minutes on `local`, 10 on `claude`. Expiry parks the
  task on WAIT "no progress for N min" and leaves the session alive, so you
  can read what it was doing. The timer and the watches belong to the
  session: `x` and `d` cancel them with it.
- **GREEN.** `R` runs on the fast model as today, into
  `*tasks-review:<branch>*`, its prompt asking for a first line
  `VERDICT: ok` or `VERDICT: concerns`. The verdict does not block anything;
  it is the Board's prefix for the REVIEW task. The task is set to REVIEW
  silently — the diff is not popped while you may be elsewhere.
- **Escalation** is by hand. A WAIT reason says why; `m` flips the model,
  `M` flips and unparks. Flipping a running RED task restarts its session on
  the new model with the go line (`+tasks--restart-on-work-model`, as `y`
  does when plan and work models differ), and says so.
- **Only one task runs unattended**: exactly one in RED with a live pi
  session. `y` on a second refuses, naming the first. WAIT tasks do not
  count, so a stalled task does not block the queue. llama-server's
  `--models-max 2` has room for the interview of the next one.

## Review and the commit

`n` on a REVIEW task opens the magit diff `main...feat/x` and, while the
worktree holds uncommitted work, stages it all and opens the commit buffer
prefilled from `PLAN.md`: subject the task heading, body the `ACCEPT` line
and the ticked tests, `Co-authored-by: pi` as trailer. One commit per task.
Once the worktree is clean, `n` is `d`, unchanged: open-items check, guard
check, jailed tests, `--no-ff` merge, worktree and branch gone, DONE.

The jail still forbids pi to commit and `d` still refuses a dirty worktree;
the commit stays the point where you have read the diff.

## Being told

Two things in Emacs and one outside, for REVIEW and WAIT only; RED/GREEN
flips are silent.

- A modeline lighter, `⚑ 1 review · 1 wait`, click → Board.
- `*tasks-log*`: one line per unattended transition — time, task, from → to,
  reason. What you read after a WAIT.
- A macOS notification (`osascript -e 'display notification …'`, no new
  dependency) with the heading and the new state.

## Board

| state | prefix |
|---|---|
| `PLAN` | `plan?` / `approve!` (as now) |
| `RED` | `3/5 · 4m` — ticks, minutes since last progress; `try 2/4` after a bounce |
| `REVIEW` | the `VERDICT:` word from `R` |
| `WAIT` | `:REASON:` (as now) |

## Models

`+ai-pi-model-args` gains `("local-think" "--model" "qwopus-reason")` on the
`local` network; the skeleton's `PLAN_MODEL_ALL` defaults to it, so the
interview runs on the reasoning model and the work on `qwopus-coder`.

## This repo

`$DOOMDIR` is the testbed, not the only target; the flow is for any project
with a `tasks.org`. Two things make it work here:

- the walkthrough's `.dir-locals.el` setting `projectile-project-test-cmd`
  gets committed, so `t` and `d` are non-interactive;
- `init.el`, `packages.el` and recipe changes stay human-merged: they need
  `doom sync` and a restart, which no unattended step does.

This change itself touches `modules/my/tasks/` and `modules/my/ai/`, so `d`
asks about it as an agent guard. That is the guard working.

## Decisions

- Q: Which gates survive? → A: `s`, `y`, the commit, `d`; docs updated to say so.
- Q: How does Emacs learn pi is done? → A: `PLAN.md` ticks, plus a pi extension on `agent_settled` where pi supports it.
- Q: Who commits? → A: You, after review; the message is prepared for you.
- Q: Enforced or declared? → A: Enforced for PLAN/RED/GREEN/REVIEW/DONE; the rest hand-editable.
- Q: Which model runs each stage? → A: `local-think` for the interview, `local` for the work; `claude` by hand.
- Q: This config or any project? → A: Any project, `$DOOMDIR` as testbed; its `.dir-locals.el` committed.
- Q: Concurrency? → A: One task in development at a time.
- Q: Done vs. asking? → A: `## Done` in `PLAN.md` means done; settled without it means a question → WAIT.
- Q: RED after Done? → A: Sent back, `:TRY:` counting up to `TRIES_TOTAL` (default 4), then WAIT.
- Q: Stall? → A: 20 min `local` / 10 min `claude` without progress → WAIT, session kept.
- Q: Escalation? → A: By hand; `m` flips the model, `M` flips and unparks.
- Q: The commit? → A: `n` on REVIEW stages and prefills the commit from the plan; one commit per task.
- Q: Does `R` gate? → A: No; `VERDICT:` shown on the Board.
- Q: Being told? → A: macOS notification, plus a modeline lighter and `*tasks-log*` in Emacs.
- Q: Where the extension lives? → A: In the config, passed by `--extension`.
- Q: Auto-REVIEW pops the diff? → A: No; `n` opens it. One line to flip.
- Q: Where the design goes? → A: This file; README stage table and the walkthrough table updated.
