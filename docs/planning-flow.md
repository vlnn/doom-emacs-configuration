# Planning flow: PLAN as a gate

Status: decided 2026-10-17 after a grilling session; implemented the same
day on `feat/plan-as-a-gate`, one commit per step of the order of work
below. This document is the design: the PLAN stage produces a decision
rather than a label. The agent interviews you about the task (the grilling
loop), writes `PLAN.md`, and waits; you approve from `tasks.org` or the
Board; only then does it write code. The same interview is how the agent
settles any decision later on, instead of guessing. README › Tasks › The
plan is the user-facing summary; `docs/flow-walkthrough.md` § 3 shows a
round.

What landed differs from the text below in three small ways:

- The plan code lives in `modules/my/tasks/plan.el`, loaded from
  `config.el`, with its tests in `test/tasks-plan-test.el`; `config.el` was
  long enough.
- `+tasks/approve-plan`'s `n` resumes the session before prefilling the
  refusal, so point lands in the input as described.
- The model restart on `y` goes through `+tasks--hand-off` with a new
  `:send` flag on `+ai--launch-pi`, so the go line is sent once the new
  session is up rather than left in its input.

## Why

Today `SPC 5 s` leaves a task in PLAN and opens pi with `/feature <heading>`.
Whatever planning happens lives in the chat buffer: nothing stores it, review
cannot check the diff against it, and `SPC 5 n` on a PLAN task runs the tests,
which is meaningless before any test exists. `/feature` already says "stop and
wait for my go", with nothing written to say go against. PLAN is the cheapest
point to redirect or abort, and the flow gives you nothing to redirect against.

A plan the agent writes alone is its guesses, neatly formatted. An interview
inverts that: the agent maps the decisions the task forces, looks up the facts
itself, asks you only what you alone can settle, and stops at a confirmation
gate.

## Goals

1. Every started task has a `PLAN.md` you approved before implementation.
2. The plan is an artefact the rest of the flow reads: the AI review checks the
   diff against it; `done` refuses while it has open items.
3. Decisions the plan did not settle go through the same interview at any
   stage, and land in the same file.
4. The interview can run on a better model than the implementation; the plan
   is the handoff.
5. Nothing widens what agents may do: the skill, `AGENTS.md` and the plan file
   all sit where the jail, the trust review and the merge guard already look.

## Non-goals

- Tracking the agent's liveness or progress from the Board.
- Replacing `ACCEPT`. The acceptance line stays the one-line contract; the
  plan is how the agent intends to meet it.
- Mid-session model switching. A stage change that changes the model restarts
  the session; `PLAN.md` carries the decisions across.
- Making ticked tests a precondition of `done`. The count is shown, not
  enforced; a local model that forgets to tick would turn `C-u d` into a habit.

## Facts the design rests on

Checked against pi 1.3.0 (`badlogic/pi-mono` at `c268154`), pilish, and this
repo on 2026-10-17.

- A prompt template cannot invoke a skill: pi expands `/name` by argument
  substitution and sends the text; `/skill:grilling` inside a template body
  reaches the model as literal text. The reliable user route to a skill is
  `/skill:<name> [args]`; arguments are appended to the loaded instructions as
  a user request.
- `.agents/skills/` is discovered from the working directory upward, stopping
  at the first directory holding a `.git` file or directory; a linked
  worktree's root has a `.git` file, so pi reads the worktree's own copy and
  never the main checkout's. Project skills load only after trust; trust is
  keyed by canonical path with parent inheritance, so `.worktrees/<x>` under a
  trusted repo inherits it.
- Precedence: project beats `~/.pi/agent`; `.pi/skills` beats `.agents/skills`;
  first name wins with a collision diagnostic.
- Plain pi has no sub-agents. `grilling`'s "dispatch a sub-agent for facts"
  has nothing to call; the model looks things up itself with read, grep and
  bash. No jail change.
- Upstream `grill-me` is one line, "Call the Skill tool with grilling", a
  Claude Code primitive pi lacks. Only `grilling` is worth shipping; it is the
  model-invocable one of the family.
- `.git/info/exclude` lives in the common dir, so one entry covers every
  worktree. `git status --porcelain` and `git worktree remove` ignore excluded
  files: an excluded `PLAN.md` blocks neither `d`'s dirty check nor the
  worktree removal. `+tasks--exclude-from-git` runs on every
  `+tasks--add-worktree`, so existing projects pick a new entry up on their
  next `s`.
- `+tasks/ai-review` runs with `default-directory` set to the main checkout;
  `PLAN.md` must be read from `(+tasks--task-worktree-of task)`.
- Restarting a session on another model exists: `+ai--stop-pilish-session`
  then `+ai--start-pi` with `:model`.
- `pilish-send` sends the input buffer and queues a follow-up when pi is busy,
  so "send" is prefill-if-empty plus `pilish-send`; the kill ring fallback
  for a busy input stays as it is.
- This repo has no `AGENTS.md` and no `.agents/`; `+tasks-guarded-files`,
  `+ai-trust` and `agent.sb` already cover both.
- `+ai-pi-model-args` knows `local` (`qwopus-coder`) and `claude`;
  `qwopus-reason` is served but has no `:MODEL:` value.
- The README's stage table already says RED means "pi is working".

## Stage semantics after this change

| state | means | enters by | leaves by |
|---|---|---|---|
| PLAN | session open; interview running, or `PLAN.md` awaiting approval | `s`; `n` on WAIT when the plan is not approved | `n` → `y` (to RED); `x` |
| RED | plan approved, pi is working; or tests fail | `n` → `y`; `t` fails; `n` on WAIT when the plan is approved | `t` passes |

One meaning per keyword: approval is a state change, not a property. The Board
prefix for a PLAN task shows `plan?` while the file is missing and `approve!`
once it exists without an `Approved:` line, in place of the branch; other
states keep showing the branch.

## Pieces

### 1. The `grilling` skill

```sh
npx skills@latest add mattpocock/skills -a pi -s grilling --copy
```

writes `.agents/skills/grilling/SKILL.md`; `--copy` matters, since a symlink
into npx's cache is what the trust review would otherwise be reviewing. Commit
it verbatim together with `.agents/skills/README.md` naming the upstream
commit (`49dd158` at the time of writing), the install line, and why the file
lives under the guard. Updates re-run the line and are read as a magit diff.

`grill-me` is not shipped. `+tasks/init` offers (asking, never silently) to
copy the skill into a new project.

### 2. `s` starts the interview; `/feature` retires

`+tasks--feature-prompt` becomes `+tasks--interview-prompt`:

```
/skill:grilling <heading>

PART OF: <parents>            ; when nested
ACCEPT: <the acceptance line>

When the frontier is empty, write PLAN.md at the repo root:
<the format below, rendered from +tasks--plan-format>
Leave "## Open" bare when nothing is unresolved. Then say
"PLAN.md written; waiting for approval." and stop.
```

The format paragraph is generated from the same constant the parser's regexps
are built from, so the shape has one owner. `~/.pi/agent/prompts/feature.md`
is deleted once this lands; its TDD rules move to `AGENTS.md` (piece 5).

### 3. `PLAN.md` format

Fixed headings, validated by Emacs before approval is offered:

```markdown
# Plan: <heading>

ACCEPT: <the acceptance line>

## Decisions
- Q: <question as asked> → A: <the user's answer>

## Tests, in order
- [ ] `<test name>`: <X> should <Y>

## Functions
- `<name>` in `<file>`: <one line>

## Touches guarded files
- none | <paths>

## Open
```

- `Approved: <date>` is appended by Emacs on `y`, after the `ACCEPT` line.
  It is the one approval record: the agent can read it from the worktree,
  `tasks.org` cannot be seen from there.
- *Decisions* grows for the life of the task: a mid-task interview appends its
  `Q → A` lines there, in the same form, no separate section.
- The agent ticks tests as they go green; `r` and `d` show the count.
- *Open* is empty when it has no bullet at all; any bullet stops `d`.

### 4. Jail and git housekeeping

- `PLAN.md` at the worktree root is under `PROJECT`, matches none of the
  denied name patterns, and needs no profile change.
- `+tasks--excluded-files` gains `PLAN.md`.
  Test: `+tasks--excluded-files/keeps-the-plan-out-of-git`.
- `+tasks--guarded-p` is unchanged: `.agents/` and `AGENTS.md` are already
  guarded, so steps that add them go through the guard prompt on `done`.

### 5. `AGENTS.md`

New in this repo; the jail keeps pi from editing it, so every line is yours.
Contents:

- House rules, lifted from `/feature`: `+scope/command`, `+scope--helper`, no
  comments unless the why is non-obvious, every `should` inside
  `ert-info "X should Y"`, `bin/test` and `bin/compile-check` after each
  change, no staging or committing.
- The gate, scoped by the file so plain `SPC 3` sessions are not caught:
  > While `PLAN.md` exists at the repo root without an `Approved:` line, edit
  > no file other than `PLAN.md`.
- The standing rule:
  > Any decision this task forces that `PLAN.md` does not settle, during
  > planning or later, goes through the grilling interview
  > (`.agents/skills/grilling/SKILL.md`), never a guess. Append each answer to
  > `## Decisions` before continuing. Say so when a round changes the test
  > list.
- The correction for pi:
  > There are no sub-agents here. Look facts up yourself with read, grep and
  > bash.
- The TDD loop: write the next unticked test from `PLAN.md`, run the tests,
  show the failure, smallest change that passes, tick it, repeat; finish with
  a summary of the diff and how `ACCEPT` is met.

`+tasks/init` offers (asking) an `AGENTS.md` for a new project holding only
the gate, the standing rule, the sub-agent line and the `PLAN.md` paragraph.
Coding style and the test command are that project's business; a stale test
command in a file the agent cannot fix is a trap.

### 6. Emacs: `SPC 5 n` on PLAN approves

`+tasks--next-steps`: `("PLAN" . +tasks/approve-plan)`; RED keeps
`+tasks/test`.

`+tasks/approve-plan`:

1. Finds the task (`+tasks--work-task`) and its worktree.
2. Without `PLAN.md`: `+tasks/resume`, and `No PLAN.md yet; the interview is
   still running`.
3. With a file missing a required heading: `user-error` naming it
   (`PLAN.md has no "## Open"`), nothing else happens.
4. Otherwise opens `PLAN.md` read-only in a side window next to the chat and
   asks `Approve this plan? (y, n, e)`:
   - `y`: appends `Approved: <date>`, sets the task to RED, and sends
     `Plan approved: PLAN.md carries the Approved line. Start with its first
     test; any decision it does not settle goes through the grilling
     interview first.` When `:PLAN_MODEL:` differs from `:MODEL:` (piece 8)
     the session is stopped and started again on `:MODEL:` before the line is
     sent; that is why the line names the file.
   - `n`: prefills `Plan not approved: ` and leaves point in the input.
   - `e`: opens `PLAN.md` writable; a buffer-local `after-save-hook` sends
     `PLAN.md edited; read it again and tell me if anything no longer holds,
     then wait`. The task stays PLAN; `n` again when ready.

Sending is `+ai--send-pilish-input`: prefill when the input is empty, then
`pilish-send`; a busy input gets the kill ring and the message, as today.

`+tasks/unpark` goes WAIT → RED when `PLAN.md` is approved, else PLAN.
`+tasks/resume` on a dead session uses `:PLAN_MODEL:` until the plan is
approved and `:MODEL:` after. A later decision never reopens the gate: the
round you answered is the approval.

`+tasks--agenda-prefix`: for a PLAN task, `plan?` / `approve!` by the file,
instead of the branch.

New functions, each small: `+tasks--plan-file`, `+tasks--plan-p`,
`+tasks--plan-approved-p`, `+tasks--plan-missing-heading`,
`+tasks--plan-mark`, `+tasks--approve`, `+tasks--show-plan`,
`+tasks--read-approval`, `+tasks--edit-plan`, `+ai--send-pilish-input`.

Tests (ERT, `test/tasks-test.el`; file fixtures in a temp worktree):

- `+tasks--next-step/plan-asks-for-approval-not-tests`
- `+tasks--plan-missing-heading/names-the-first-absent-section`
- `+tasks--plan-approved-p/reads-the-approved-line`
- `+tasks--agenda-prefix/shows-plan-marks-instead-of-the-branch`
- `+tasks/approve-plan/resumes-when-the-plan-is-not-written-yet`
- `+tasks/approve-plan/refuses-a-plan-without-the-fixed-headings`
- `+tasks/approve-plan/approves-into-red-and-sends-the-go-line`
- `+tasks/approve-plan/prefills-a-refusal`
- `+tasks/approve-plan/sends-the-reread-line-after-an-edit`
- `+tasks/approve-plan/restarts-on-the-work-model-when-it-differs`
- `+tasks/unpark/returns-to-red-when-the-plan-is-approved`

### 7. Review and done read the plan

- `+tasks--review-prompt` gains `Plan the agent committed to:` followed by
  `PLAN.md` (read from the worktree), before the diff; the question becomes
  "what does not serve the criterion, what departs from the plan without
  saying so, missing or weak tests, risky changes".
  Test: `+tasks--review-prompt/carries-the-plan-when-there-is-one`.
- `+tasks/review` echoes `plan: 4/5 tests ticked` after `ACCEPT` and the
  guard note. Test: `+tasks--plan-tick-count/counts-checked-tests`.
- `+tasks/done` refuses while `## Open` has any bullet, with the lines in the
  message; `C-u SPC 5 d` merges anyway. Without `PLAN.md` nothing changes.
  Tests: `+tasks/done/refuses-while-the-plan-has-open-items`,
  `+tasks/done/merges-with-a-prefix-arg-despite-open-items`,
  `+tasks/done/merges-an-unplanned-branch-as-before`.
- `+tasks/abort` deletes nothing extra: `PLAN.md` goes with the worktree.

### 8. A model per stage

`:PLAN_MODEL:`, read like `:MODEL:` through `+ai-pi-model-args`; absent, it
is `:MODEL:`, so the interview runs on `local` unless you say otherwise.
`C-u SPC 5 s` asks for it (completion over `+ai-pi-model-args`) and stores it;
plain `s` asks nothing new. Skeleton gains
`#+PROPERTY: PLAN_MODEL_ALL local claude` next to `MODEL_ALL`.
`+ai--check-pi-model` runs on both before the review.

`s` and `c` start a session on `:PLAN_MODEL:` while the plan is not approved;
`y` restarts it on `:MODEL:` when the two differ (piece 6).

Tests: `+tasks--task-plist/carries-the-plan-model`,
`+tasks--interview-model/falls-back-to-the-work-model`,
`+tasks/start/asks-for-the-plan-model-with-a-prefix-arg`,
`+tasks/start/opens-the-interview-on-the-plan-model`.

Open after the first real task: whether `qwopus-coder` runs a usable
interview. If not, add `("local-think" "--model" "qwopus-reason")` to
`+ai-pi-model-args` (loopback network like `local`) and make it the usual
`:PLAN_MODEL:`.

### 9. Docs

- README › Tasks: the stage table's PLAN and RED rows, `n` on PLAN, the
  `plan?`/`approve!` marks, `:PLAN_MODEL:` and `C-u s`, `PLAN.md` among the
  excluded files, the `done` refusal and its prefix arg, `AGENTS.md` and
  `.agents/skills/` as guarded inputs.
- `docs/flow-walkthrough.md`: setup step 2 (`/feature`) becomes the skill
  copy and `AGENTS.md`; § 3 PLAN shows one real round of questions for the
  bug it walks through, the resulting `PLAN.md`, and `SPC 5 n` → `y`;
  § 5 shows a mid-task round appended to *Decisions*.
- `.agents/skills/README.md` (piece 1).
- This document: status line flipped to implemented, with the step that did
  each piece.

## Order of work

Each step is a commit with its tests first; each leaves the flow usable.

1. `PLAN.md` excluded from git; the Board shows `plan?` / `approve!` for PLAN
   tasks (pieces 4, 6-prefix). No behaviour change for anyone who never
   writes the file. Done in `9155ac1`.
2. `n` on PLAN approves: heading validation, `y` → `Approved:` + RED + send,
   `n`, `e`, `unpark` (piece 6). `/feature` unchanged: you can still ask pi
   for a `PLAN.md` by hand, and approval already works. Done in `510629d`.
3. Review and done read the plan (piece 7). Pure-function work, no guarded
   files. Done in `ab83663`.
4. `grilling` into `.agents/skills/`, `AGENTS.md`, the `/skill:grilling`
   draft, `init`'s offer (pieces 1, 2, 5). The first step the trust review
   sees, and the one that makes the interview happen by default. Delete
   `~/.pi/agent/prompts/feature.md` afterwards. Done in `741eb19`.
5. `:PLAN_MODEL:` (piece 8). Done in `c7b8b4b`.
6. Docs (piece 9), finished alongside each step, collected here.

Tasks started before step 4 keep working: `n` on a PLAN task with no
`PLAN.md` resumes pi, and `/feature` answers until it is deleted.

## Decided against

- `:PLAN: approved` on the heading: RED already says it, and the agent cannot
  see `tasks.org` from the worktree; the `Approved:` line is the record.
- Staying PLAN after approval until `t`: three meanings for one keyword and
  `n` re-asking for approval.
- A three-way `plan ✓` mark: never visible once approval moves the task.
- Shipping `grill-me`, or a project `feature` skill that reads `grilling`:
  one relies on a Claude Code primitive, the other on the model following a
  "read this file" sentence; `/skill:grilling` is the documented route.
- Pre-seeding `PLAN.md` from Emacs: "file exists" would stop meaning
  "interview done".
- Ticked tests as a `done` precondition; a `nothing` bullet under *Open*;
  a third prompt on every `s` for the model; a per-project test command in
  the `AGENTS.md` that `init` offers.
