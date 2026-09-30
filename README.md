# Doom Emacs configuration

Private `$DOOMDIR` for Doom Emacs. Clojure, Python (uv + pylsp), Dyalog APL
via ride-apl, Forth, and a local llama-server behind gptel.

## Layout

`config.el` loads one file per concern, in the order listed at its top
(`macos` → `secrets` → `appearance` → … → `snippets`). Package declarations
live in `packages.el`, module flags in `init.el`.

| file | owns |
|---|---|
| `appearance.el` | fonts, theme, cursor, scrolling |
| `completion.el` | corfu, abbrev, indentation |
| `evil.el` | `jkl;` motion layout, REPL initial states |
| `keychords.el` | every `key-chord` binding |
| `avy.el` + `avy-functions.el` | avy dispatch actions (`+avy--*`) |
| `lsp.el` / `python.el` | global lsp-mode settings / pylsp + REPL |
| `dape.el` | debugpy through uv, `SPC d d` smart debug |
| `ai.el` | llama-server backend, gptel, annotate, aider, ai-code, opencode |
| `apl.el`, `forth.el` | language setups mirroring Doom's cider wiring |
| `casual.el` | casual transient menus |
| `snippets.el` + `snippets/` | yasnippet helpers (`+snippets-body`) and snippets |

Naming follows Doom: `+scope/command` (interactive), `+scope--helper`
(private), `+scope-var`.

## Secrets

Copy `secrets.el.example` to `secrets.el` (git-ignored) for API keys and
machine-local values. Missing `secrets.el` is tolerated.

## Tests

```sh
bin/test          # ERT, plain `emacs --batch`, no Doom needed
```

`test/doom-stubs.el` provides no-op `after!`/`map!`/`use-package!` so config
files load outside Doom; tests cover the pure functions
(`+snippets--*`, `+dape--*`, `+ai--annotate-*`, `+ai--llama-model-ids`).

## Local models

See `llama-server/README.md`. `M-x +ai/refresh-llama-models` re-reads the
served model list into gptel.
