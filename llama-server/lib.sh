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

preset_hf_models() {
  local ini="$1"
  sed -n 's/^hf *= *\([^:]*\):\{0,1\}\(.*\)$/\1 \2/p' "$ini"
}

hf_include_pattern() {
  local tag="$1"
  [[ -n "$tag" ]] && echo "*$tag*" || echo "*.gguf"
}

blob_is_intact() {
  local blob="$1"
  [[ "$(basename "$blob")" == "$(shasum -a 256 "$blob" | cut -c1-64)" ]]
}

lfs_blobs() {
  local cache="${LLAMA_MODELS_DIR:-$HOME/.cache/llama.cpp}"
  find "$cache" -path '*/blobs/*' -type f -name '[0-9a-f]*' -size +1M 2>/dev/null | grep -E '/[0-9a-f]{64}$' || true
}

hf_cli() {
  command -v hf 2>/dev/null || command -v huggingface-cli 2>/dev/null
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

model_status() {
  python3 -c '
import json, sys
name = sys.argv[1]
for m in json.load(sys.stdin)["data"]:
    if m["id"] == name:
        s = m["status"]
        print("failed" if s.get("failed") else s["value"])
        break
else:
    print("unknown")
' "$1"
}

fetch_model_status() {
  curl -fsS "$(server_url)/models" | model_status "$1"
}

model_progress() {
  python3 -c '
import json, sys
name = sys.argv[1]
for m in json.load(sys.stdin)["data"]:
    if m["id"] == name and m.get("progress"):
        done = sum(p["done"] for p in m["progress"].values())
        total = sum(p["total"] for p in m["progress"].values())
        gib = 2 ** 30
        print(f"{done / gib:.2f} / {total / gib:.2f} GiB  {100 * done / total:.1f}%")
' "$1"
}

fetch_model_progress() {
  curl -fsS "$(server_url)/models" | model_progress "$1"
}

cache_size() {
  du -sh "${LLAMA_MODELS_DIR:-$HOME/.cache/llama.cpp}" 2>/dev/null | cut -f1
}

loading_progress() {
  local model="$1" progress
  progress="$(fetch_model_progress "$model")"
  echo "${progress:-loading, cache at $(cache_size)}"
}

model_action() {
  local action="$1" model="$2"
  curl -fsS -X POST "$(server_url)/models/$action" -H 'Content-Type: application/json' -d "{\"model\": \"$model\"}" >/dev/null
}

wait_until_loaded() {
  local model="$1" status
  while :; do
    status="$(fetch_model_status "$model")"
    case "$status" in
      loaded) printf '\r\033[K'; return 0 ;;
      failed|unknown|unloaded) printf '\r\033[K  %s: %s\n' "$model" "$status" >&2; return 1 ;;
      downloading|loading) printf '\r\033[K  %s' "$(loading_progress "$model")" ;;
      *) printf '\r\033[K  %s' "$status" ;;
    esac
    sleep 5
  done
}
