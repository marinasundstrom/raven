#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
temporary_root="${TMPDIR:-/tmp}"
output_directory="$(mktemp -d "$temporary_root/raven-native-aot-smoke.XXXXXX")"
log_file="$(mktemp "$temporary_root/raven-native-aot-smoke.XXXXXX.log")"
ilverify_directory="$(mktemp -d "$temporary_root/raven-native-aot-ilverify.XXXXXX")"

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

  case "$ilverify_directory" in
    "$temporary_root"/raven-native-aot-ilverify.*)
      if [[ -d "$ilverify_directory" && ! -L "$ilverify_directory" ]]; then
        rm -rf -- "$ilverify_directory"
      fi
      ;;
  esac
}
trap cleanup EXIT

compiler="$repo_root/src/Raven.Compiler/bin/Debug/net10.0/rvnc.dll"
project="$repo_root/samples/projects/greenhouse-monitor/GreenhouseMonitor.rvnproj"

if [[ "${FORCE_REBUILD:-0}" == "1" || ! -f "$compiler" ]]; then
  "$repo_root/scripts/codex-build.sh"
fi

dotnet "$compiler" \
  "$project" \
  --configuration Release \
  --ilverify \
  --output "$ilverify_directory"

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
