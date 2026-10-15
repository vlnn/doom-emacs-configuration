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
| `ai` | llama-server backend, gptel, `annotate.el`, aider, ai-code, opencode |
| `notes` | denote, ob-duckdb |
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
```

Both run in GitHub Actions on Emacs 29 and 30 (`.github/workflows/test.yml`);
the compile check is advisory there. `doom sync && doom doctor` stays a
local step.

`test/doom-stubs.el` provides no-op `after!`/`map!`/`use-package!` so config
files load outside Doom; tests cover the pure functions
(`+snippets--*`, `+dape--*`, `+avy--eval-*`, `+ai--annotate-*`, `+ai--llama-model-ids`, `+github-explorer--*`, `+lua--*`).
The `+lua-repl/*` tests drive a real `lua -i` through comint and are skipped
when no Lua interpreter is on `PATH`.

## Local models

See `llama-server/README.md`. `M-x +ai/refresh-llama-models` re-reads the
served model list into gptel.
