# Walkthrough: the config fixes itself

A realistic end-to-end run of the agent task flow (`tasks` module + pilish +
`trust.el`) where the repo being worked on is this `$DOOMDIR`. The bug is a
real one in `+tasks/start`, found by using the flow.

## The flow in one table

| state | who moves it there | how |
|---|---|---|
| `IDEA` | you | `SPC 5 i` (capture into `* Ideas`) |
| `TODO` | `+tasks/start` | `SPC 5 s`: branch, `:BRANCH:`, `:ACCEPT:`, `:MODEL:`, refile to `* Active`, open pi |
| `PLAN` → `RED` → `GREEN` | you, as the agent reports | `SPC m t` in `tasks.org`; each change is timestamped (`!`) |
| `REVIEW` | you, when the agent says it is done | shows up in `SPC o A r` ("Needs my review") |
| `DONE` / `DROPPED` | you, after merge or abandon | `DROPPED` asks for a note (`@`) |

Only `IDEA → TODO` is automated. The in-flight states are a log of what you saw,
not something pi updates; the Board (`SPC 5 o`) and "Last week" (`SPC o A w`)
views read them.

## One-time setup for this repo

1. `M-x +tasks/init` in `$DOOMDIR` writes `tasks.org`. Keep it out of git so
   branch switches never touch it:
   `echo tasks.org >> .git/info/exclude` (same for `tasks.org_archive`).
2. `/feature` is a pi prompt template and does not live in this repo. Put it in
   `~/.pi/agent/prompts/feature.md`, for example:

   ```markdown
   ---
   description: Work the tasks.org entry named by the argument, TDD
   argument-hint: <heading>
   ---
   Find the heading "$@" in tasks.org at the repo root. Its :ACCEPT: line is the
   definition of done.

   1. PLAN: list the failing ERT tests to write, in order, and the small named
      functions they need. Stop and wait for my go.
   2. RED: add the first test under test/, run bin/test, show the failure.
   3. GREEN: smallest change that passes; run bin/test and bin/compile-check.
   4. Repeat 2–3 per test. Do not commit. Finish with a summary of the diff
      and how :ACCEPT: is met.

   Follow README.md: +scope/command, +scope--helper, no comments unless the
   why is non-obvious, every should wrapped in ert-info "X should Y".
   ```

   Run `/reload` in an open pi session after editing it.
3. Optional `AGENTS.md` at the root with the same house rules. Adding one makes
   `SPC 3` show the trust report once (pi has no decision for `$DOOMDIR` yet);
   pick `t`.

With no `.pi/`, `AGENTS.md` or `justfile`, `SPC 3` starts pi without a report:
there is nothing for `trust.el` to review.

## 1. The trigger

You tag the `* Active` heading in some project's `tasks.org` (`* Active :work:`)
and press `SPC 5 s` on an IDEA there. Emacs reports `Search failed`, and:

- git is already on the new `feat/...` branch;
- the IDEA vanished from the buffer (unsaved).

Cause, in `modules/my/tasks/config.el`: `+tasks--move-under` deletes the subtree
*before* `+tasks--top-heading-end` looks for `^\* Active[ \t]*$`, which a tagged
heading does not match. `+tasks/start` also switches branches before anything
that can fail.

Recover first: `u` in `tasks.org` brings the task back,
`git switch - && git branch -d feat/<slug>` undoes the empty branch.

## 2. Capture and start

In `$DOOMDIR`:

1. `SPC 5 i`, heading `Start drops the task if Active is tagged`, `C-c C-c`.
2. The bug is in the command you are about to use, so take the tag off
   `* Active` in *this* `tasks.org` first (it is untagged in a fresh skeleton).
3. Point on the IDEA, `SPC 5 s`:
   - `Branch:` prefilled `feat/start-drops-the-task-if-active-is-tagged`
     (slug capped at 40 chars); accept or shorten.
   - `ACCEPT for "…":` e.g. `start refiles under a tagged * Active; with none it errors before git switch and loses nothing`.
   - The entry gets `:BRANCH:`, `:ACCEPT:`, `:MODEL: local`, becomes `TODO`,
     moves to `* Active`; the `branches` block gains a row; the file is saved.
   - pilish opens at the project root with `/feature Start drops the task if
     Active is tagged` in the input; the same line is on the kill ring, and
     text you had already typed there is never replaced.

`:MODEL:` is a label for the Board and review; pi uses whatever model its own
settings select.

## 3. PLAN

Send the prefilled input. A good plan for this bug:

1. `+tasks--top-heading-end` finds a tagged `* Active`.
2. With no `* Active`, it signals a `user-error` naming the heading.
3. `+tasks--move-under` leaves the buffer untouched when the target is missing
   (look the target up before deleting).
4. `+tasks/start` checks the target heading before `+tasks--checkout-branch`.

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

The agent edits `+tasks--top-heading-end` and `+tasks--move-under`, and adds the
check to `+tasks/start`; `bin/test` passes, `bin/compile-check` is clean.
`global-auto-revert-mode` keeps any open buffers of those files current.
`SPC m t` → `GREEN`.

Batch tests prove the pure functions; the running Emacs still has the old
definitions. Load the new ones without restarting: open the file, `C-M-x` on
each changed `defun` (or `SPC h r r` for module-level changes). Then dogfood
`:ACCEPT:` by hand: tag `* Active` again, capture a throwaway IDEA, `SPC 5 s`,
check it lands under `* Active :work:`; delete it and its branch.

## 6. REVIEW

`SPC m t` → `REVIEW`; it now shows in `SPC o A r`.

- `SPC g g`: the diff, rendered by delta.
- A second opinion on the diff: gptel with the `review` directive.
- A dense hunk: select it, `SPC o l E` for line-by-line notes.
- Stage, commit (gptel-magit drafts the message from the staged diff),
  merge to `main`, delete the branch.

## 7. Close

`SPC m t` → `DONE` (logs `CLOSED`). On save the `branches` row disappears with
the branch. Archive with `SPC m A` when the `* Active` list gets long; entries go
to `tasks.org_archive` under `* Archived`.

## When the fix breaks Emacs

pilish and hax both run inside the Emacs you are changing. Before restarting:

- `bin/test && bin/compile-check` from a terminal;
- if Emacs no longer starts: `git -C $DOOMDIR switch main`, then
  `doom doctor`; `emacs --debug-init` to see where it stops;
- for changes to `init.el`, `packages.el` or package recipes, run `doom sync`
  and restart, and keep pi in a terminal for that session instead of `SPC 3`.

## Sharp edges seen on the way

- `/feature` and its behaviour live in `~/.pi/agent/prompts/`, not here; a
  fresh machine has the key binding but not the prompt.
- `trust.el` treats commits under your `user.email` as yours. An agent that
  commits with your git identity can change `AGENTS.md` or `.pi/` without the
  "changed by someone else" report; read those diffs in magit.
- `+tasks/start` writes `tasks.org` after switching branches. With `tasks.org`
  tracked in git, its state differs per branch; that is why setup excludes it.
- In-flight states are manual, so the Board is only as fresh as your last
  `SPC m t`.
