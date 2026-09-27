# llama-server for ai.el

`config.ini` is the single source of truth for local models: one section per
served name (`qwopus-coder`, `qwopus-reason`, ...). `ai.el` asks the running
server for that list at startup, so Emacs never drifts from what is installed.

```sh
./install.sh            # brew install llama.cpp, link config.ini, start launchd agent
./sync.sh               # compare config.ini with what the server serves
./sync.sh --restart     # after editing config.ini
./sync.sh --prefetch    # download every model now instead of on first request
./test.sh               # unit tests for the shell helpers
```

Models referenced by `hf =` are downloaded into `~/.cache/llama.cpp` on first
use (override with `LLAMA_MODELS_DIR=... ./install.sh`). Point a section at a
local file instead with `model = /path/to/file.gguf`.

Inside Emacs, `M-x my/llama-server-refresh-models` re-reads the list without a
restart. Logs land in `~/Library/Logs/llama-server.log`.
