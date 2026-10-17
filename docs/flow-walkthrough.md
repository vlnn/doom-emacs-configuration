# Walkthrough: the config fixes itself

A realistic end-to-end run of the agent task flow (`tasks` module + pilish +
`trust.el` + the agent jail) where the repo being worked on is this `$DOOMDIR`. The bug is a
real one in `+tasks/start`, found by using the flow.

> The fix this run produces has since landed: `* Active` may carry tags,
> statistics cookies or a priority, and a `tasks.org` without one stops
> `SPC 5 s` before the review, with nothing changed. The steps below still
> show the flow; to replay them, take any open IDEA instead of this bug.

## The flow in one table

| state | who moves it there | how |
|---|---|---|
| `IDEA` | you | `SPC 5 i` (capture into `* Ideas`), `SPC 5 I` (sub-idea under the task at point) |
| `TODO` | `+tasks/start` | `SPC 5 s` on a leaf: repo review if due, branch in its own worktree, `:BRANCH:`, `:ACCEPT:`, `:MODEL:`, refile its tree to `* Active`, jailed pi session there named after the branch |
| `PLAN` → `RED` → `GREEN` | you, as the agent reports | `SPC m t` in `tasks.org`; each change is timestamped (`!`) |
| `REVIEW` | you, when the agent says it is done | `SPC 5 r` / `SPC 5 R`; listed in `SPC o A r` ("Needs my review") |
| `DONE` | `+tasks/done` | `SPC 5 d`: guard check, jailed tests in the worktree, merge `--no-ff`, remove worktree and branch, DONE |
| `DROPPED` | you, on abandon | `SPC m t`; asks for a note (`@`) |

`TODO` and `DONE` are set for you; the states in between are a log of what you
saw, not something pi updates. The Board (`SPC 5 o`) and "Last week"
(`SPC o A w`) read them.

## One-time setup for this repo

1. `M-x +tasks/init` in `$DOOMDIR` writes `tasks.org` and adds it,
   `tasks.org_archive` and `.worktrees/` to `.git/info/exclude`. Task worktrees
   live in `.worktrees/`, so the checkout Emacs loads stays on `main`.
2. `/feature` is a pi prompt template and does not live in this repo. Put it in
   `~/.pi/agent/prompts/feature.md`, for example:

   ```markdown
   ---
   description: Work one tasks.org task, TDD
   argument-hint: <heading>
   ---
   Work on this task from tasks.org (in the main checkout; from a task
   worktree that is ../../tasks.org):

   $@

   PART OF names its parent tasks: they say why it exists; implement only this
   task, not its parents or siblings. ACCEPT is the definition of done; if it is
   missing above, read :ACCEPT: from the heading in tasks.org.

   1. PLAN: list the failing ERT tests to write, in order, and the small named
      functions they need. Stop and wait for my go.
   2. RED: add the first test under test/, run bin/test, show the failure.
   3. GREEN: smallest change that passes; run bin/test and bin/compile-check.
   4. Repeat 2–3 per test. Do not stage or commit (the jail refuses it anyway).
      Finish with a summary of the diff and how ACCEPT is met.

   Follow README.md: +scope/command, +scope--helper, no comments unless the
   why is non-obvious, every should wrapped in ert-info "X should Y".
   ```

   Run `/reload` in an open pi session after editing it; the reloaded pi keeps
   the session's jail, model and trust flags.
3. Tell `SPC 5 t` and `SPC 5 d` how to test this repo. Without a `justfile` they
   ask projectile, which has no test command for a Doom config. Add
   `.dir-locals.el` at the root (Emacs may ask once whether to accept it):

   ```elisp
   ((nil . ((projectile-project-test-cmd . "bin/test"))))
   ```

   Commit it, so every task worktree has it too. Without it, `SPC 5 d` asks
   whether to merge with no tests run. Agents cannot rewrite it: the jail keeps
   dir-locals read-only, since Emacs evaluates them with your rights.
4. Optional `AGENTS.md` at the root with the same house rules, committed for
   the same reason. Adding one makes the first `SPC 3` / `SPC 5 s` show the
   review in `*agent-review:…*` (pi has no decision for `$DOOMDIR` yet);
   `C-c C-c` trusts it. Task worktrees share that review.
5. On the Mac, `bin/jail-check` once: it runs probes under
   `modules/my/ai/agent.sb` and should end with `jail behaves as agent.sb says`.

`:MODEL: local` runs pi with `--model qwopus-coder` and loopback-only network,
`claude` with Anthropic's Sonnet and the network open (`+ai-pi-model-args`,
`+ai-pi-model-networks`); both apply when a task's session starts.

## 1. The trigger

You tag the `* Active` heading in some project's `tasks.org` (`* Active :work:`)
and press `SPC 5 s` on an IDEA there. Once the review lets pi start, Emacs
reports `Search failed`, and:

- `.worktrees/feat-…` and its `feat/...` branch already exist;
- the IDEA vanished from the buffer (unsaved);
- pi did not open.

Cause, in `modules/my/tasks/config.el`: `+tasks--start-at` adds the worktree
before anything that can fail, then `+tasks--move-under` deletes the subtree
*before* `+tasks--top-heading-end` looks for `^\* Active[ \t]*$`, which a tagged
heading does not match.

Recover first: `u` in `tasks.org` brings the task back,
`git worktree remove .worktrees/feat-<slug> && git branch -d feat/<slug>`
undoes the empty worktree and branch.

## 2. Capture and start

In `$DOOMDIR`:

1. `SPC 5 i`, heading `Start drops the task if Active is tagged`, `C-c C-c`.
2. The bug is in the command you are about to use, so take the tag off
   `* Active` in *this* `tasks.org` first (it is untagged in a fresh skeleton).
3. Point on the IDEA, `SPC 5 s`:
   - `Branch:` prefilled `feat/start-drops-the-task-if-active-is-tagged`
     (slug capped at 40 chars); accept or shorten.
   - `ACCEPT for "…":` e.g. `start refiles under a tagged * Active; with none it errors before adding the worktree and loses nothing`.
   - The repo review, if due. Nothing has changed yet; `q` cancels cleanly.
   - Then the entry gets `:BRANCH:`, `:ACCEPT:`, `:MODEL: local`, becomes
     `TODO`, moves to `* Active` (with its whole tree, had it been nested); the
     `branches` block gains a row (`merged` until the first commit, since the
     empty branch is already in `main`); the file is saved.
   - `.worktrees/feat-start-drops-…` is created on the new branch; `$DOOMDIR`
     itself stays on `main`.
   - A pi session named `feat/start-drops-…` opens in that worktree, jailed:
     it can write only there (plus temp and its session files), on the local
     model with loopback-only network, with `/feature Start drops the task if Active is tagged` and
     its `ACCEPT:` line in the input (on the kill ring instead if the input
     already had text).

If the task looks too big for one branch, split it instead of step 3: with
point on the IDEA, `SPC 5 I` once per part (say `Tagged Active in
top-heading-end` and `Guard start before adding the worktree`). The IDEA is now a
parent, so `SPC 5 s` on it offers only those parts, deepest first; each part
gets its own branch, worktree and session, and its draft carries a
`PART OF: Start drops the task if Active is tagged` line. The first start moves
the parent, still IDEA, under `* Active`; `tasks.org` refuses to close it until
every part is DONE or DROPPED, after which it can be started itself.

Coming back later, from another buffer or after a restart: `SPC 5 c` opens
the task's worktree and focuses its session. A restart is safe mid-task: Doom
loads `$DOOMDIR`, which never left `main`. `SPC 3` inside pi hides it.

## 3. PLAN

Send the prefilled input. A good plan for this bug:

1. `+tasks--top-heading-end` finds a tagged `* Active`.
2. With no `* Active`, it signals a `user-error` naming the heading.
3. `+tasks--move-under` leaves the buffer untouched when the target is missing
   (look the target up before deleting).
4. `+tasks--start-at` checks the target heading before `+tasks--add-worktree`.

`SPC m t` → `PLAN`. Push back now if a step is missing; it is the cheapest
point to redirect.

## 4. RED

The agent adds tests to `test/tasks-test.el` and runs `bin/test`:

```elisp
(ert-deftest +tasks--top-heading-end/accepts-tagged-heading ()
  (with-temp-buffer
    (insert "* Ideas\n* Active :work:\n** TODO x\n* Backlog\n")
    (org-mode)
    (ert-info ("top-heading-end should find a top heading that carries tags")
      (should (= (save-excursion (goto-char (point-min))
                                 (re-search-forward "^\\* Backlog")
                                 (line-beginning-position))
                 (+tasks--top-heading-end "Active"))))))

(ert-deftest +tasks--move-under/keeps-subtree-when-target-missing ()
  (with-temp-buffer
    (insert "* Ideas\n** IDEA keep me\n")
    (org-mode)
    (goto-char (point-min))
    (re-search-forward "^\\*\\* IDEA")
    (beginning-of-line)
    (should-error (+tasks--move-under "Active") :type 'user-error)
    (ert-info ("move-under should not delete the subtree when the target heading is missing")
      (should (string-match-p "IDEA keep me" (buffer-string))))))
```

Both fail (`search-failed`, not `user-error`). `SPC m t` → `RED`.

## 5. GREEN

The agent edits `+tasks--top-heading-end`, `+tasks--move-under` and
`+tasks--start-at` in its worktree; `SPC 5 t` on the task runs `bin/test` there,
in the jail, in a compilation buffer, and
`bin/compile-check` stays clean. `global-auto-revert-mode` keeps any open
buffers of those files current. `SPC m t` → `GREEN`.

Batch tests prove the pure functions; the running Emacs still has the old
definitions, and loading new ones runs the agent's code with your rights, so
read the diff first (`SPC 5 r`, next section). Then open the file in the
worktree and `C-M-x` each changed `defun`, or `M-x load-file` it for
module-level changes; `SPC h r r` would reload `$DOOMDIR`, still on `main`.
Dogfood `ACCEPT` by hand: tag `* Active` again, capture a throwaway IDEA,
`SPC 5 s`, check it lands under `* Active :work:`; delete it, its worktree,
branch and session.

## 6. REVIEW

`SPC m t` → `REVIEW`; it now shows in `SPC o A r`. With point on the task (or
in any buffer of its worktree):

- `SPC 5 r`: magit diff `main...feat/start-drops-…`, the task's `ACCEPT` in the
  echo area, and `Touches agent guards: modules/my/tasks/config.el, …`: this
  fix edits the flow itself, which is exactly what deserves a slow read.
- `SPC 5 R`: a fresh local-model review of that diff against `ACCEPT` in
  `*tasks-review:feat/…*`, the guard note on top; it has not seen the pi
  conversation.
- A dense hunk: select it, `SPC o l E` for line-by-line notes.
- The agent could not commit: stage and commit in magit, in the worktree
  (gptel-magit drafts the message from the staged diff).

## 7. Done

`SPC 5 d` with point on the task:

1. refuses if `$DOOMDIR` is dirty, or the worktree holds uncommitted or
   untracked work (a test file the agent wrote and you forgot to add);
2. asks before merging, since the diff touches agent guards;
3. runs `bin/test` in the worktree, in the jail, and stops on failure;
4. merges `--no-ff` into `main` as `Merge feat/start-drops-…: Start drops…`;
5. removes the worktree and deletes the branch (asks about `origin/…` if it
   exists);
6. marks the task `DONE`, logging `CLOSED`, and refreshes the `branches` block.

Quit the task's pi session afterwards; its worktree is gone.

Nothing is pushed: `git push` from magit when you are happy. A row reading
`DONE! delete branch` is a branch left over from a task finished by hand.
Archive with `SPC m A` when the `* Active` list gets long; entries go to
`tasks.org_archive` under `* Archived`.

## When the fix breaks Emacs

pilish and hax run inside the Emacs you are changing, but the agent's code
reaches it only when you load it (`C-M-x`, `load-file`) or merge and restart.
Before restarting after a merge:

- `bin/test && bin/compile-check` from a terminal in `$DOOMDIR`;
- if Emacs no longer starts: `git -C $DOOMDIR reset --keep HEAD~1` undoes the
  unpushed merge, then `doom doctor`; `emacs --debug-init` to see where it
  stops;
- for changes to `init.el`, `packages.el` or package recipes, run `doom sync`
  and restart, and keep pi in a terminal for that session instead of `SPC 3`;
  that pi is not jailed unless you start it under `sandbox-exec` the way
  `bin/jail-check` does.

## Sharp edges seen on the way

- `/feature` and its behaviour live in `~/.pi/agent/prompts/`, not here; a
  fresh machine has the key binding but not the prompt.
- `trust.el` treats commits from your `user.email` and `+ai-trust-own-emails` as
  yours. A jailed agent cannot commit or touch the context files (`AGENTS.md`,
  `AGENTS.override.md`, `CLAUDE.md`), `.pi/` or `.agents/`; one you run
  unjailed (Linux, a remote project, a terminal) can, under your identity,
  without the "changed by someone else" report. Read those diffs in magit; the
  guard on `SPC 5 r`/`d` points at them.
- The review for a task worktree is its repo's: it scans the main checkout. Agent
  inputs someone else committed on the task branch show up in the guard note,
  not in the review, until the merge.
- The jail is macOS Seatbelt; elsewhere Emacs says once that agents run with
  your full rights. When the jail denies something an agent legitimately
  needs, `log stream --style compact --predicate 'sender == "Sandbox"'` names
  the path; widening `agent.sb` is itself a guarded change.
- In-flight states are manual, so the Board is only as fresh as your last
  `SPC m t`.
- A parent with open parts cannot be DROPPED either: drop the parts first, or
  set `:NOBLOCKING: t` on it.
- `SPC 5 d` merges into your local `main` only; the remote learns nothing until
  you push.
