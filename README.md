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
| `ai` | llama-server backend, gptel, `annotate.el`, aider, ai-code, opencode, pilish (`SPC 3`), hax (`SPC 4`), `trust.el` |
| `notes` | denote, ob-duckdb |
| `tasks` | per-repo `tasks.org`: agenda views, `branches` block, `p i` idea capture, `+tasks/init` |
| `apl`, `forth` | language setups mirroring Doom's cider wiring |
| `dirvish` | dired/dirvish |
| `vc` | magit-todos, magit-delta, why-this, github-explorer |
| `snippets` | yasnippet helpers (`+snippets-body`); snippet files stay in `snippets/` |

Adding a feature = new directory + one line in `init.el`; removing one is the
reverse, and `doom sync` drops its packages with it.

Naming follows Doom: `+scope/command` (interactive), `+scope--helper`
(private), `+scope-var`.

## Secrets

Copy `secrets.el.example` to `secrets.el` (git-ignored) for API keys and
machine-local values. Missing `secrets.el` is tolerated.

## Tests

```sh
bin/test           # ERT, plain `emacs --batch`, no Doom needed
bin/compile-check  # byte-compile every config file with Doom macros stubbed
```

Both run in GitHub Actions on Emacs 29 and 30 (`.github/workflows/test.yml`);
the compile check is advisory there. `doom sync && doom doctor` stays a
local step.

`test/doom-stubs.el` provides no-op `after!`/`map!`/`use-package!` so config
files load outside Doom; tests cover the pure functions
(`+snippets--*`, `+dape--*`, `+avy--eval-*`, `+ai--annotate-*`, `+ai--llama-model-ids`, `+github-explorer--*`,
`+tasks--*`, `+ai-trust--*`).

## Coding agents

`SPC 3` starts [pilish](https://github.com/dnouri/pilish) (pi in Emacs) at the
project root; `SPC 4` starts [hax](https://github.com/OleksandrChekhovskyi/hax)
in eat, if it is on `PATH`. Both first go through `modules/my/ai/trust.el`:
before the first start in a repo, when pi has no trust decision for it, and
after someone else commits to what an agent reads (`.pi/`, `.agents/skills`,
any `AGENTS.md`, the `justfile`), a report lists those files and flags risky
lines. Your choice for pi is written to `~/.pi/agent/trust.json`; pilish runs
with `pilish-project-trust-policy` `'default`, so that file decides what loads.
Essential tree-sitter grammars install on first use; the `*pilish-grammars*`
buffer opens in Emacs state so its `i`/`q` keys work under evil.
Reviewed commits are kept in `doom-cache-dir`.

## Tasks

Each repo in the agent flow keeps a `tasks.org` at its root; `M-x +tasks/init`
writes the skeleton. `SPC 5` (and `SPC m w` inside `tasks.org`) opens the
tasks menu:

| key | does |
|---|---|
| `i` | capture an IDEA into this project's `tasks.org` |
| `s` | start the IDEA/TODO at point (or pick one): create and switch to `feat/<slug>`, set `:BRANCH:`, ask `:ACCEPT:` if missing, default `:MODEL:` to `local`, IDEA → TODO, refile under `* Active`, then open pi with `/feature <heading>` in an empty input (also on the kill ring) |
| `a` | set `:ACCEPT:` |
| `l` | on a `branches` row: link that branch to a task, or to a new IDEA named after it |
| `b` | refresh the `branches` block (also done on every save) |
| `o` / `f` | Board agenda / visit `tasks.org` |

The `branches` block only matches headings with a TODO keyword, so a stray
`:BRANCH:` on `* Branches` shows as `NONE`. `org-agenda-files` is extended, not
replaced: on every agenda command it is reset to `org-directory` plus the
`tasks.org` of each known projectile project (remote projects skipped).
`+tasks-branch-prefixes` (default `("feat/")`, safe as a dir-local) picks the
branches the block lists; with more than one prefix, `start` asks which.

## Local models

See `llama-server/README.md`. `M-x +ai/refresh-llama-models` re-reads the
served model list into gptel.
