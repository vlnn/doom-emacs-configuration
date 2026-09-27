#!/usr/bin/env bash

LLAMA_HOST="${LLAMA_HOST:-127.0.0.1}"
LLAMA_PORT="${LLAMA_PORT:-8080}"
LLAMA_LABEL="org.ggml.llama-server"

render_plist() {
  local template="$1" binary="$2" config_ini="$3" models_dir="$4" log="$5"
  sed -e "s|@LLAMA_SERVER@|$binary|g" \
      -e "s|@CONFIG_INI@|$config_ini|g" \
      -e "s|@MODELS_DIR@|$models_dir|g" \
      -e "s|@LOG@|$log|g" \
      "$template"
}

preset_model_names() {
  local ini="$1"
  sed -n 's/^\[\(.*\)\]$/\1/p' "$ini" | grep -vx '\*'
}

served_model_names() {
  python3 -c 'import json, sys; print("\n".join(m["id"] for m in json.load(sys.stdin)["data"]))'
}

missing_models() {
  local wanted="$1" served="$2"
  comm -23 <(sort <<<"$wanted") <(sort <<<"$served")
}

server_url() {
  echo "http://$LLAMA_HOST:$LLAMA_PORT"
}

server_is_up() {
  curl -fsS -m 2 "$(server_url)/health" >/dev/null 2>&1
}

wait_for_server() {
  local tries="${1:-30}"
  until server_is_up; do
    ((tries-- > 0)) || return 1
    sleep 1
  done
}

fetch_served_models() {
  curl -fsS "$(server_url)/v1/models" | served_model_names
}
