# Doom Emacs configuration

Private `$DOOMDIR` for Doom Emacs. Clojure, Python (uv + pylsp), Dyalog APL
via ride-apl, Forth, and a local llama-server behind gptel.

## Layout

Every feature is a private Doom module under `modules/my/<name>/`, enabled by
the `:my` block at the end of `init.el` (listed in load order). Each module
owns its `config.el` and, when it needs packages, a `packages.el`; the root
`packages.el` only keeps `unpin!`/`disable-packages!` overrides.

| module | owns |
|---|---|
| `system` | macOS shell/modifiers, `secrets.el`, global auto-revert |
| `appearance` | fonts, theme, cursor, scrolling, mini-ontop, beacon |
| `editing` | corfu/abbrev, `jkl;` evil layout, key chords, avy actions, projectile, small editing packages |
| `casual` | casual transient menus |
| `lsp` | global lsp-mode settings, flycheck, jsonian |
| `python` | uv REPL, pylsp, dape (`SPC d d` smart debug) |
| `ai` | llama-server backend, gptel, `annotate.el`, `flymake.el` (AI review, `SPC o l r`), aider, ai-code, opencode, pilish (`SPC 3`), hax (`SPC 4`), `trust.el`, `jail.el` + `agent.sb` |
| `notes` | denote, ob-duckdb |
| `tasks` | per-repo `tasks.org`: agenda views, `branches` block, a worktree per task, `p i` idea capture, `+tasks/init` |
| `apl`, `forth` | language setups mirroring Doom's cider wiring |
| `lua` | lua-mode REPL run by `evaluator.lua` (not `lua -i`) with inline results, file-based interrupts, cider-shaped localleader, busted runners; LÖVE games run the REPL in-game |
| `dirvish` | dired/dirvish |
| `vc` | magit-todos, magit-delta, why-this, github-explorer |
| `snippets` | yasnippet helpers (`+snippets-body`); snippet files stay in `snippets/` |

Adding a feature = new directory + one line in `init.el`; removing one is the
reverse, and `doom sync` drops its packages with it.

Naming follows Doom: `+scope/command` (interactive), `+scope--helper`
(private), `+scope-var`.

## LÖVE

`love.*` only exists inside a running game, so a LÖVE project needs this line
at the top of its `conf.lua` (`M-x +lua/add-love-hook` puts it there):

```lua
if os.getenv("EMACS_LOVE_REPL") then dofile(os.getenv("EMACS_LOVE_REPL")) end
```

It does nothing unless Emacs started the game. With it in place, `SPC m '` or
any eval starts the game as the REPL: evals run between frames on the main
thread, and the game's output shows up in `*lua*`. `+lua-love-command` says how
to run LÖVE.

## Secrets

Copy `secrets.el.example` to `secrets.el` (git-ignored) for API keys and
machine-local values. Missing `secrets.el` is tolerated.

## Tests

```sh
bin/test           # ERT, plain `emacs --batch`, no Doom needed
bin/compile-check  # byte-compile every config file with Doom macros stubbed
bin/jail-check     # macOS: run probes under agent.sb, check what the jail allows
```

The first two run in GitHub Actions on Emacs 29 and 30
(`.github/workflows/test.yml`); the compile check is advisory there.
`bin/jail-check` needs a Mac, and `doom sync && doom doctor` stays a local
step.

`test/doom-stubs.el` provides no-op `after!`/`map!`/`use-package!` so config
files load outside Doom; tests cover the pure functions
(`+snippets--*`, `+dape--*`, `+avy--eval-*`, `+ai--annotate-*`, `+ai--flymake-*`, `+ai--llama-model-ids`, `+github-explorer--*`,
`+tasks--*`, `+ai-trust--*`, `+ai--jail*`); the tasks tests drive real git in
temp repos for worktrees and `done`. ERT checks the arguments the jail is
started with and, as Emacs regexps, which paths the profile's name rules
match; `bin/jail-check` checks what Seatbelt enforces, so run it after
editing `agent.sb`. `test/run.el` turns native-comp trampolines off, so tests
that stub primitives (`message`, `read-string`, `call-process`) need no working
native compiler; your Emacs still does for its own native compilation.

## Coding agents

`SPC 3` starts [pilish](https://github.com/dnouri/pilish) (pi in Emacs) at the
project root; `SPC 4` starts [hax](https://github.com/OleksandrChekhovskyi/hax)
in eat, if it is on `PATH`. Both first go through `modules/my/ai/trust.el`:
before the first start in a repo, when pi has no trust decision for it, and
after someone else commits to what an agent reads (`.pi/`, `.agents/skills`,
any `AGENTS.md`, `AGENTS.override.md` or `CLAUDE.md`, the `justfile`; names
in any case, as pi and just read them), a report lists those files and flags
risky lines. The report is a normal grep buffer, so its lines jump to the files; decide
there with `C-c C-c` (trust / proceed), `C-c C-u` (untrusted once), `C-c C-d`
(do not trust) or `C-c C-k` / `q` (cancel). A session that is already running
skips the review, since its inputs are loaded; `SPC 3` inside pi hides it.
Add other machines' commit emails to `+ai-trust-own-emails` so your own
commits never count as someone else's. Your choice for pi is written to `~/.pi/agent/trust.json`; pilish runs
with `pilish-project-trust-policy` `'default`, so that file decides what loads,
except "untrusted once", which starts that session with `--no-approve`, reloads
included. Reviews are
kept per agent, so a pi review never lets hax start unreviewed.
Essential tree-sitter grammars install on first use; the `*pilish-grammars*`
buffer opens in Emacs state so its `i`/`q` keys work under evil.
Reviewed commits are kept in `doom-cache-dir`. In a task worktree both agents
share the review of the repo it belongs to.

## Agent jail

On macOS, pi, hax and the tests `SPC 5 t`/`SPC 5 d` run through `sandbox-exec`
with `modules/my/ai/agent.sb` (`modules/my/ai/jail.el` builds the command). The
profile is read from the live config, so an agent editing its project's copy
changes nothing until you merge and reload.

The jail hooks into pilish where it starts pi (`pilish--start-process`), so
every pi process pilish starts is jailed: `SPC 3`, task sessions, `M-x pilish`,
and restarts (`/reload`, `C-c C-p R`, a session revived after its process
died). A restart keeps what the session started with: its jail network, its
`:MODEL:` flags and its trust flag. A pi pilish starts on its own gets the open
network and your plain pilish settings. Should pilish stop starting pi through
that function, `SPC 3` and task starts refuse instead of running pi unjailed.

In the jail:

- writes reach only the project, temp dirs, `~/.pi/agent/sessions`, hax's
  state and cache, and `~/.cache/agent-jail`, where jailed runs keep their
  own uv cache and gitlibs (`UV_CACHE_DIR`, `GITLIBS`). The shared
  `~/.cache/uv`, `~/.m2` and `~/.gitlibs` stay read-only, since your REPLs
  and dape run what they hold; a test that needs a new Maven dependency
  fails until you fetch it outside (`clojure -P`) after reading the diff;
- `.git` (hooks, config, refs) of the project and of any repo nested in it,
  `.pi/`, `.agents/`, `.worktrees/`, any `AGENTS.md`, `AGENTS.override.md`,
  `CLAUDE.md` and `.dir-locals.el` (names in any case, since macOS disks
  ignore it) stay read-only: they run or load later with your rights. Agents
  read git but cannot stage or commit; that stays yours, in magit. Under the
  temp dirs repos and those files stay writable, so tests can create them as
  fixtures;
- ssh, gpg, AWS, gcloud, kube, gh, docker, npm, PyPI and cargo credentials,
  `~/.git-credentials`, `~/.authinfo*`, `~/.netrc` and any `secrets.el` are
  unreadable;
- the Emacs server socket (`emacsclient --eval` would reach your Emacs),
  ssh-agent (launchd's and whatever `SSH_AUTH_SOCK` names), gpg-agent, the
  Docker socket, LaunchServices and AppleEvents (`open`, `osascript`) are
  off limits;
- a `:MODEL: local` task session reaches only llama-server's port 8080 on
  loopback, with no name lookups, so a local provider in pi's `models.json`
  must use `127.0.0.1`, not `localhost` (`+ai-pi-model-networks`); every other
  session has the network open.

The pi opened to record a trust decision runs outside, since it writes
`~/.pi/agent/trust.json`. Without `sandbox-exec` (Linux) or for remote
projects agents start as before, and Emacs says so once;
`(setq +ai-jail-enabled nil)` turns the jail off. When something an agent needs
is denied, `log stream --style compact --predicate 'sender == "Sandbox"'` names
the path; widen `agent.sb` (a guarded file, see `d` below) and rerun
`bin/jail-check`.

The jail covers what runs inside it. Merging, `C-M-x` on the agent's elisp
and anything else you run on its output afterwards run with your rights; the
guard on `r`/`R`/`d` and reading the diff are what stand there. Reads outside
the denied paths, including other repos, stay open.

## Tasks

Each repo in the agent flow keeps a `tasks.org` at its root; `M-x +tasks/init`
writes the skeleton and adds `tasks.org` and `tasks.org_archive` to
`.git/info/exclude`, so they stay out of git without touching `.gitignore`.
Each started task works in its own git worktree, `.worktrees/<branch>` (slashes
become `-`, also excluded, and left out of lsp's file watching in the main
checkout), so the main checkout stays on main. For `$DOOMDIR`
that is the config Emacs loads: a restart mid-task never loads the agent's code.
`SPC 5` (and `SPC m w` inside `tasks.org`) opens the tasks menu:

| key | does |
|---|---|
| `i` | capture an IDEA into this project's `tasks.org` |
| `I` | add a sub-IDEA under the open task at point (or a picked one) |
| `n` | next: do whatever moves the task at point (or the board row at point) one stage on — see the stage table below; a heading without a keyword is refused |
| `s` | start the leaf IDEA/TODO at point (on a parent: pick one of its leaves; elsewhere: pick any leaf): create `feat/<slug>` in a new worktree, set `:BRANCH:`, ask `:ACCEPT:` if missing, default `:MODEL:` to `local`, IDEA/TODO → PLAN, refile its whole tree under `* Active`, then open a pi session in that worktree, named after the branch, on the task's `:MODEL:`, with `/feature <heading>`, a `PART OF:` line for nested tasks and its `ACCEPT:` line in the input (on the kill ring instead when the input has text). Nothing changes if the repo review is cancelled or `tasks.org` has no top-level `Active` heading (tags, cookies and a priority on it are fine); a task already linked keeps its `:BRANCH:` |
| `c` | continue a started task: open its worktree (adding one for a branch without) and focus its pi session |
| `a` | set `:ACCEPT:` (ideas and in-flight tasks) |
| `w` | wait: park the task on something outside the repo, asking why; the reason is kept in `:REASON:` and shown on the Board until the task moves again |
| `r` | magit diff `main...feat/x` for the task, task → REVIEW; its `:ACCEPT:` is echoed, with a guard note when the diff touches agent guards (below) |
| `R` | AI review of that diff against `:ACCEPT:` (gptel `review` directive, fast model) into `*tasks-review:<branch>*`, the guard note first; task → REVIEW |
| `t` | run the tests of the tree at hand in the jail: the task's worktree on a task in `tasks.org`, else the current project. The command is that tree's: `just test` when its justfile has a `test` recipe and `just` is installed, else projectile's test command with that tree's dir-locals applied (so a committed `.dir-locals.el` setting `projectile-project-test-cmd` works from any buffer); says so when `just` is missing. A task's run leaves it GREEN or RED when the compilation finishes |
| `d` | done: refuse if the main checkout is dirty or the worktree holds uncommitted or untracked work, ask before merging a diff that touches agent guards, run the worktree's tests there in the jail (same command lookup as `t`; stop on failure), merge `--no-ff` into main as `Merge feat/x: <heading>`, remove the worktree, delete the branch (asks about `origin`), mark DONE. Failing tests leave the task RED; a merge that cannot finish (main cannot fast-forward to origin, conflicts) leaves it WAIT with the obstacle as `:REASON:`. Nothing is pushed; quit the task's pi session afterwards |
| `x` | abort: throw the agent's work away. Asks, naming how many commits go; ends the task's pi session and every process it started; removes the worktree, uncommitted files included; deletes the branch, keeping its tip at `refs/aborted/<branch>` for reading; returns the heading to the state it was started from (its own state log, else TODO), dropping `:BRANCH:` and `:REASON:`, keeping `:ACCEPT:` and `:MODEL:`. Refuses a branch main already contains: that is `d`'s job |
| `l` | on a `branches` row: link that branch to a task, or to a new IDEA named after it |
| `b` | refresh the `branches` block (also done on every save) |
| `o` / `O` / `f` | Board agenda / flip the Board between this project and every project (the agenda's category filter; `|` clears it) / visit `tasks.org` |

### Stages

The keywords are the pipeline, and the commands move tasks along it; typing a
keyword by hand works too (`SPC m t`), but then nothing else happens.

| state | means | set by | `n` does |
|---|---|---|---|
| `IDEA` | captured, not committed to | `i`, `I`, `l` | `s` |
| `TODO` | ready: a leaf you intend to start | you | `s` |
| `PLAN` | started: worktree, branch, pi session exist | `s`, `n` on WAIT | `t` |
| `RED` | last test run failed | `t`, `d` | `t` |
| `GREEN` | last test run passed | `t` | `r` |
| `REVIEW` | you are reading the diff | `r`, `R` | `d` |
| `WAIT` | parked on something outside the repo; `:REASON:` says what | `w`, `d`, a keyword change by hand (asks why) | back to PLAN, reason dropped, pi resumed |
| `DONE` / `DROPPED` | merged / abandoned | `d` / you | archive the subtree |

Every transition is timestamped (`!`), so the "Last week" agenda view is a log
of the flow. `x` is the way back: it undoes `s`.

The Board (`SPC 5 o`) is one block per stage in that order, ideas at the top,
DONE and DROPPED gathered at the bottom as the pile to archive. Each row shows
the task's branch, or its reason while it waits. Ready and Ideas list leaves
only. `SPC 5 O` narrows the Board to the project it was opened from and back.

The `branches` block only matches headings with a TODO keyword, so a stray
`:BRANCH:` on `* Branches` shows as `NONE`. `org-agenda-files` is extended, not
replaced: every agenda command adds the `tasks.org` of each known projectile
project (remote projects skipped) to whatever it holds, `org-directory` when
it holds nothing, and drops the ones a project no longer has; each file's
agenda category is its repo name. The Board (`b`), Last week (`w`) and Needs
my review (`r`) views join `org-agenda-custom-commands`, replacing only
commands of yours under those keys.
`+tasks-branch-prefixes` (default `("feat/")`, safe as a dir-local) picks the
branches the block lists; with more than one prefix, `start` asks which.
`:MODEL:` maps to pi flags through `+ai-pi-model-args` (`local`, `claude`);
they apply when a task's session starts. A value missing from that list stops
`s` and `c` before the review, rather than starting pi on its default model
with the network open.

Ideas nest: a sub-heading with a TODO keyword is a part of its parent, and
the deepest open ones go first. Only leaves (no open sub-task) start; pickers
list trees in file order with each tree's deepest leaves first, and the
Board's Ready/Ideas sections skip parents until their children close. In
`tasks.org` a parent cannot be marked DONE or DROPPED while a sub-task is open
(`org-enforce-todo-dependencies`, buffer-local); `:NOBLOCKING: t` on a heading
opts out. Starting a leaf moves its whole tree, ancestors still IDEA, under
`* Active`; each leaf gets its own branch, and a parent becomes startable once
its parts are closed.

Agent guards are the files whose change widens what agents may do:
`modules/my/ai/` and `modules/my/tasks/` (the jail, the review and this flow),
`.pi/`, `.agents/`, any `AGENTS.md`, `AGENTS.override.md`, `CLAUDE.md`,
`.dir-locals.el` and the justfile, in any case (`+tasks-guarded-files`). The
jail keeps agents off `.pi/`, `.agents/`, the context files and dir-locals;
the guard catches their edits to the rest, and whatever you or others commit. A branch the old flow left checked
out in the main checkout keeps working there; `d` then tests it there, as
before.

The block's State column reads `ahead N` (commits not on main), `merged`, or
`DONE! …` once the task is done; `DONE! delete branch` marks leftovers. Main
is `origin/HEAD`, else `main`, else `master`. `r`, `R`, `t` and `d` act on the
linked task at point, else the task linked to the `branches` row at point,
else the task on the current branch (so any buffer of a worktree), else ask;
so does `c`.

`docs/flow-walkthrough.md` follows one task from capture to merge, using this
repo to fix a bug in `+tasks/start`.

## Local models

See `llama-server/README.md`. `M-x +ai/refresh-llama-models` re-reads the
served model list into gptel.

## AI review through Flymake

`SPC o l r` (`+ai-flymake-mode`) sends the buffer, lines numbered, to a model
and shows what comes back as Flymake diagnostics: `error`, `warning` or `note`
on the line named, each prefixed with the reviewer's name. It runs when the
mode starts and on every save, never on edits alone; `flymake-mode` is turned
on with it and off with it. Buffers over `+ai-flymake-max-lines` (400) are
skipped with a note in `*Flymake log*`.

`SPC o l R` (`+ai/flymake-toggle-provider`) switches `+ai-flymake-provider`
between `llama` (llama-server, `+ai-fast-model`, the default) and `claude`
(`+ai-flymake-claude-model` through gptel's Anthropic backend, which needs
`ANTHROPIC_API_KEY` from `secrets.el`), re-reviewing the buffer if the mode is
on. A reply that arrives after a newer review started is dropped.

While a review is out the echo area says `AI review by llama…`, the mode's
lighter reads `AIrev[llama…]`, and doom-modeline's checker shows Flymake
running; `SPC o l ?` (`+ai/flymake-status`) asks in words. When the reply
lands the echo area says how many findings there were, or `nothing to flag`.

Each finding is written at the end of its line, `⟵ llama: …`, in the colour
of its severity: Flymake's own end-of-line display on Emacs 30
(`flymake-show-diagnostics-at-end-of-line`, set buffer-locally to `short`),
an overlay of ours on 29. The underline, fringe mark, tooltip and
`SPC o l l` (`flymake-show-buffer-diagnostics`) are Flymake's as usual.
