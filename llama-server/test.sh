#!/usr/bin/env bash
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$here/lib.sh"

failures=0

assert_eq() {
  local expected="$1" actual="$2" message="$3"
  if [[ "$expected" == "$actual" ]]; then
    echo "ok   - $message"
  else
    echo "FAIL - $message"
    echo "       expected: $expected"
    echo "       actual:   $actual"
    failures=$((failures + 1))
  fi
}

assert_contains() {
  local needle="$1" haystack="$2" message="$3"
  if [[ "$haystack" == *"$needle"* ]]; then
    echo "ok   - $message"
  else
    echo "FAIL - $message"
    echo "       missing: $needle"
    failures=$((failures + 1))
  fi
}

test_render_substitutes_placeholders() {
  local rendered
  rendered="$(render_plist "$here/org.ggml.llama-server.plist.in" /usr/local/bin/llama-server /home/u/.config/llama.cpp/config.ini /home/u/models /tmp/llama.log)"
  assert_contains "<string>/usr/local/bin/llama-server</string>" "$rendered" "render_plist should substitute the llama-server binary path"
  assert_contains "<string>/home/u/.config/llama.cpp/config.ini</string>" "$rendered" "render_plist should substitute the preset path"
  assert_contains "<string>/home/u/models</string>" "$rendered" "render_plist should substitute the models dir"
  assert_contains "<string>/tmp/llama.log</string>" "$rendered" "render_plist should substitute the log path"
  assert_eq "" "$(grep -o '@[A-Z_]*@' <<<"$rendered" || true)" "render_plist should leave no placeholders behind"
}

test_preset_model_names_lists_sections() {
  local ini
  ini="$(mktemp)"
  printf '[*]\nctx-size = 1\n\n[alpha]\nhf = x\n\n[beta:7b]\nhf = y\n' >"$ini"
  assert_eq $'alpha\nbeta:7b' "$(preset_model_names "$ini")" "preset_model_names should list every section except the global one"
  rm -f "$ini"
}

test_served_model_names_parses_v1_models() {
  local json='{"object":"list","data":[{"id":"alpha","object":"model"},{"id":"beta:7b","object":"model"}]}'
  assert_eq $'alpha\nbeta:7b' "$(served_model_names <<<"$json")" "served_model_names should extract ids from /v1/models json"
}

test_missing_models_reports_difference() {
  assert_eq "gamma" "$(missing_models $'alpha\nbeta\ngamma' $'alpha\nbeta')" "missing_models should list preset names the server does not serve"
  assert_eq "" "$(missing_models $'alpha' $'alpha\nbeta')" "missing_models should be empty when the server serves everything in the preset"
}

test_render_substitutes_placeholders
test_preset_model_names_lists_sections
test_served_model_names_parses_v1_models
test_missing_models_reports_difference

if ((failures > 0)); then
  echo "$failures failure(s)"
  exit 1
fi
echo "all tests passed"
