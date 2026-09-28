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

test_model_status_reads_router_status() {
  local json='{"data":[{"id":"alpha","status":{"value":"loaded"}},{"id":"beta","status":{"value":"downloading","failed":false}},{"id":"gamma","status":{"value":"unloaded","failed":true,"exit_code":1}}]}'
  assert_eq "loaded" "$(model_status alpha <<<"$json")" "model_status should read the status value of a served model"
  assert_eq "downloading" "$(model_status beta <<<"$json")" "model_status should report in-progress downloads"
  assert_eq "failed" "$(model_status gamma <<<"$json")" "model_status should report failed over the raw status"
  assert_eq "unknown" "$(model_status delta <<<"$json")" "model_status should report unknown for models the server does not list"
}

test_model_progress_sums_files() {
  local json='{"data":[{"id":"alpha","status":{"value":"downloading"},"progress":{"u1":{"done":1073741824,"total":2147483648},"u2":{"done":0,"total":2147483648}}},{"id":"beta","status":{"value":"loading"}}]}'
  assert_eq "1.00 / 4.00 GiB  25.0%" "$(model_progress alpha <<<"$json")" "model_progress should sum done and total over all files of a model"
  assert_eq "" "$(model_progress beta <<<"$json")" "model_progress should print nothing for a model without download progress"
}

test_preset_hf_models_lists_repo_and_tag() {
  local ini
  ini="$(mktemp)"
  printf '[*]\nctx-size = 1\n\n[a]\nhf = org/repo-GGUF:Q4_K_M\n\n[b]\nmodel = /x.gguf\n\n[c]\nhf = ggml-org/gpt-oss-20b-GGUF\n' >"$ini"
  assert_eq $'org/repo-GGUF Q4_K_M\nggml-org/gpt-oss-20b-GGUF ' "$(preset_hf_models "$ini")" "preset_hf_models should list repo and tag for every hf section, empty tag when absent"
  rm -f "$ini"
}

test_hf_include_pattern_matches_tag_or_everything() {
  assert_eq "*Q4_K_M*" "$(hf_include_pattern Q4_K_M)" "hf_include_pattern should glob around the quant tag"
  assert_eq "*.gguf" "$(hf_include_pattern "")" "hf_include_pattern should take all ggufs when no tag is given"
}

test_preset_hf_models_lists_repo_and_tag
test_hf_include_pattern_matches_tag_or_everything
test_model_progress_sums_files
test_render_substitutes_placeholders
test_preset_model_names_lists_sections
test_served_model_names_parses_v1_models
test_missing_models_reports_difference
test_model_status_reads_router_status

if ((failures > 0)); then
  echo "$failures failure(s)"
  exit 1
fi
echo "all tests passed"
