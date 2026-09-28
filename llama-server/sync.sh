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
    loading|downloading) echo "  already in progress, attaching" ;;
    *) model_action load "$model" || return 1 ;;
  esac
  wait_until_loaded "$model" && model_action unload "$model" && echo "  done"
}

ensure_hf_cli() {
  hf_cli >/dev/null && return
  command -v uv >/dev/null || { echo "need the hf CLI: brew install uv, or pipx install 'huggingface_hub[cli]'" >&2; exit 1; }
  uv tool install -q 'huggingface_hub[cli]' >&2
  hf_cli >/dev/null || { echo "hf CLI installed but not on PATH; run: uv tool update-shell" >&2; exit 1; }
}

pull() {
  local repo="$1" tag="$2"
  echo "pulling $repo ${tag:+($tag)} ..."
  HF_HUB_DISABLE_XET=1 "$(hf_cli)" download "$repo" --include "$(hf_include_pattern "$tag")" --cache-dir "${LLAMA_MODELS_DIR:-$HOME/.cache/llama.cpp}" >/dev/null
}

pull_all() {
  ensure_hf_cli
  while read -r repo tag; do
    [[ -z "$repo" ]] || pull "$repo" "$tag"
  done < <(preset_hf_models "$here/config.ini")
  echo "downloaded; restarting the router so it picks the files up from the cache"
  restart_agent
  wait_for_server
}

verify_all() {
  local blob corrupt=0
  while IFS= read -r blob; do
    [[ -n "$blob" ]] || continue
    printf '%s ... ' "${blob#"$HOME"/}"
    if blob_is_intact "$blob"; then
      echo ok
    else
      echo CORRUPT
      corrupt=$((corrupt + 1))
    fi
  done < <(lfs_blobs)
  ((corrupt == 0)) || { echo "$corrupt corrupt blob(s): rm them and run sync.sh --pull" >&2; exit 1; }
}

unload_all() {
  while IFS= read -r model; do
    [[ -z "$model" ]] || model_action unload "$model" 2>/dev/null || true
  done < <(fetch_served_models)
}

prefetch_all() {
  while IFS= read -r model; do
    [[ -z "$model" ]] || prefetch "$model" || true
  done < <(preset_model_names "$here/config.ini")
}

main() {
  case "${1:-}" in
    --restart) restart_agent; wait_for_server ;;
    --pull) pull_all; verify_all ;;
    --verify) verify_all; exit 0 ;;
    --prefetch) prefetch_all ;;
    --unload-all) unload_all ;;
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
