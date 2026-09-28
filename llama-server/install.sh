#!/usr/bin/env bash
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$here/lib.sh"

config_dir="$HOME/.config/llama.cpp"
config_ini="$config_dir/config.ini"
models_dir="${LLAMA_MODELS_DIR:-$HOME/.cache/llama.cpp}"
log="$HOME/Library/Logs/llama-server.log"
plist="$HOME/Library/LaunchAgents/$LLAMA_LABEL.plist"

require_macos() {
  [[ "$(uname)" == Darwin ]] || { echo "install.sh targets macOS launchd; on Linux write a systemd --user unit instead" >&2; exit 1; }
}

refuse_root() {
  [[ "$(id -u)" != 0 ]] || { echo "run install.sh as your own user, not with sudo: the agent lives in ~/Library/LaunchAgents and runs in your GUI session" >&2; exit 1; }
}

ensure_llama_server_installed() {
  command -v llama-server >/dev/null || brew install llama.cpp
  command -v llama-server
}

link_preset() {
  mkdir -p "$config_dir"
  ln -sfn "$here/config.ini" "$config_ini"
}

write_plist() {
  local binary="$1"
  mkdir -p "$(dirname "$plist")" "$(dirname "$log")" "$models_dir"
  render_plist "$here/org.ggml.llama-server.plist.in" "$binary" "$config_ini" "$models_dir" "$log" >"$plist"
}

reload_agent() {
  local domain="gui/$(id -u)"
  launchctl bootout "$domain/$LLAMA_LABEL" 2>/dev/null || true
  launchctl bootstrap "$domain" "$plist"
}

report() {
  echo "llama-server is listening on $(server_url)"
  echo "preset:  $config_ini -> $here/config.ini"
  echo "models:  $models_dir"
  echo "log:     $log"
  "$here/sync.sh"
}

main() {
  require_macos
  refuse_root
  local binary
  binary="$(ensure_llama_server_installed)"
  link_preset
  write_plist "$binary"
  reload_agent
  wait_for_server || { echo "server did not come up; see $log" >&2; exit 1; }
  report
}

main "$@"
