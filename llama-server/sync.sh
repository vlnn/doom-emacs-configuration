#!/usr/bin/env bash
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$here/lib.sh"

restart_agent() {
  launchctl kickstart -k "gui/$(id -u)/$LLAMA_LABEL"
}

prefetch() {
  local model="$1"
  echo "fetching $model ..."
  case "$(fetch_model_status "$model")" in
    loaded|sleeping) echo "  already loaded"; return 0 ;;
    unknown) echo "  not known to the server (restart it after editing config.ini)"; return 0 ;;
  esac
  model_action load "$model" && wait_until_loaded "$model" && model_action unload "$model" && echo "  done"
}

prefetch_all() {
  while IFS= read -r model; do
    [[ -z "$model" ]] || prefetch "$model" || true
  done < <(preset_model_names "$here/config.ini")
}

main() {
  case "${1:-}" in
    --restart) restart_agent; wait_for_server ;;
    --prefetch) prefetch_all ;;
  esac

  server_is_up || { echo "llama-server is not running; run install.sh or sync.sh --restart" >&2; exit 1; }

  local wanted served missing
  wanted="$(preset_model_names "$here/config.ini")"
  served="$(fetch_served_models)"
  missing="$(missing_models "$wanted" "$served")"

  echo "served models:"
  sed 's/^/  /' <<<"$served"
  if [[ -n "$missing" ]]; then
    echo "in config.ini but not served (run: sync.sh --restart):"
    sed 's/^/  /' <<<"$missing"
    exit 1
  fi
}

main "$@"
