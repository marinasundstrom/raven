#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
temporary_root="${TMPDIR:-/tmp}"
output_directory="$(mktemp -d "$temporary_root/raven-native-aot-smoke.XXXXXX")"
log_file="$(mktemp "$temporary_root/raven-native-aot-smoke.XXXXXX.log")"

cleanup() {
  case "$output_directory" in
    "$temporary_root"/raven-native-aot-smoke.*)
      if [[ -d "$output_directory" && ! -L "$output_directory" ]]; then
        rm -rf -- "$output_directory"
      fi
      ;;
  esac

  case "$log_file" in
    "$temporary_root"/raven-native-aot-smoke.*.log)
      if [[ -f "$log_file" && ! -L "$log_file" ]]; then
        rm -f -- "$log_file"
      fi
      ;;
  esac
}
trap cleanup EXIT

if ! OUTPUT_DIR="$output_directory" RUN=1 \
  "$repo_root/samples/projects/greenhouse-monitor/publish-aot.sh" "$@" \
  2>&1 | tee "$log_file"; then
  echo "Native AOT publish or execution failed." >&2
  exit 1
fi

if grep -En 'warning (IL[0-9]{4}|AOT[0-9]{4})' "$log_file"; then
  echo "Native AOT publish produced trim-analysis or AOT-analysis warnings." >&2
  exit 1
fi

echo "Native AOT smoke test passed without trim-analysis or AOT-analysis warnings."
