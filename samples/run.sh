#!/usr/bin/env bash
# Run all DLLs in ./output/<tfm> except an exclude list.
# Failing one won't stop the rest.

set -Euo pipefail
shopt -s nullglob

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

DOTNET_VERSION="${DOTNET_VERSION:-net10.0}"

usage() {
  cat <<EOF
Usage: ./run.sh [options]

Options:
  -f, --framework <tfm>   Target runtime framework to execute with (default: ${DOTNET_VERSION})
  -h, --help              Show this help

Environment overrides:
  DOTNET_VERSION, OUTPUT_DIR, EXCLUSIONS_FILE, RUN_CASES_FILE
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    -f|--framework)
      [[ $# -lt 2 ]] && { echo "Missing value for $1"; exit 2; }
      DOTNET_VERSION="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1"
      usage
      exit 2
      ;;
  esac
done

# Default output is split by TFM to match build.sh and avoid cross-framework mixing.
OUTPUT_DIR="${OUTPUT_DIR:-output/$DOTNET_VERSION}"
if [[ "$OUTPUT_DIR" != /* ]]; then
  OUTPUT_DIR="$SCRIPT_DIR/$OUTPUT_DIR"
fi

TARGET_MM="${DOTNET_VERSION#net}"
FX_VERSION="$(dotnet --list-runtimes | awk -v mm="$TARGET_MM" '$1 == "Microsoft.NETCore.App" && index($2, mm ".") == 1 { latest = $2 } END { print latest }')"

if [[ -n "$FX_VERSION" ]]; then
  DOTNET_CMD=(dotnet exec --fx-version "$FX_VERSION")
  echo "Using Microsoft.NETCore.App $FX_VERSION for execution."
else
  DOTNET_CMD=(dotnet)
  echo "Warning: could not resolve installed runtime for '$DOTNET_VERSION'; using default dotnet host selection."
fi

EXCLUSIONS_FILE="${EXCLUSIONS_FILE:-$SCRIPT_DIR/exclusions.txt}"
RUN_CASES_FILE="${RUN_CASES_FILE:-$SCRIPT_DIR/run-cases.txt}"
EXCLUDE_PATTERNS=()

load_exclusions() {
  local mode="$1"
  EXCLUDE_PATTERNS=()

  [[ -f "$EXCLUSIONS_FILE" ]] || return 0

  while IFS= read -r line || [[ -n "$line" ]]; do
    line="${line#"${line%%[![:space:]]*}"}"
    [[ -z "$line" || "$line" == \#* ]] && continue

    local entry_mode pattern rest
    read -r entry_mode pattern rest <<< "$line"
    [[ -n "${rest:-}" ]] && continue
    [[ -z "${entry_mode:-}" || -z "${pattern:-}" ]] && continue

    if [[ "$entry_mode" == "all" ||
          "$entry_mode" == "$mode" ||
          ( "$mode" == "run" && "$entry_mode" == "build" ) ]]; then
      EXCLUDE_PATTERNS+=("$pattern")
    fi
  done < "$EXCLUSIONS_FILE"
}

is_excluded() {
  local relpath="$1"
  local filename="$2"
  local pattern
  for pattern in "${EXCLUDE_PATTERNS[@]-}"; do
    if [[ "$relpath" == $pattern || "$filename" == $pattern ]]; then
      return 0
    fi
  done
  return 1
}

has_source_sample() {
  local relpath="$1"
  local stem="${relpath%.dll}"

  if [[ "$stem" == _root/* ]]; then
    stem="${stem#_root/}"
  fi

  [[ -f "$SCRIPT_DIR/$stem.rav" || -f "$SCRIPT_DIR/$stem.rvn" ]]
}

load_run_case() {
  local relpath="$1"
  EXPECTED_EXIT=0
  RUN_ARGS=()

  [[ -f "$RUN_CASES_FILE" ]] || return 0

  while IFS= read -r line || [[ -n "$line" ]]; do
    line="${line#"${line%%[![:space:]]*}"}"
    [[ -z "$line" || "$line" == \#* ]] && continue

    local path expected rest
    read -r path expected rest <<< "$line"
    [[ "$path" == "$relpath" ]] || continue

    EXPECTED_EXIT="${expected:-0}"
    if [[ -n "${rest:-}" ]]; then
      read -r -a RUN_ARGS <<< "$rest"
    fi
    return 0
  done < "$RUN_CASES_FILE"
}

if [[ ! -d "$OUTPUT_DIR" ]]; then
  echo "Output directory '$OUTPUT_DIR' does not exist."
  exit 1
fi

load_exclusions "run"

dlls=()
while IFS= read -r dll; do
  dlls+=("$dll")
done < <(find "$OUTPUT_DIR" -type f -name "*.dll" | sort)

if (( ${#dlls[@]} == 0 )); then
  echo "No .dll files found in '$OUTPUT_DIR'."
  exit 0
fi

failures=()
successes=()

for dll in "${dlls[@]}"; do
  filename="$(basename "$dll")"
  relpath="${dll#"$OUTPUT_DIR"/}"
  if [[ "$relpath" == "$dll" ]]; then
    relpath="$filename"
  fi

  if ! has_source_sample "$relpath"; then
    echo "Skipping non-sample assembly: $relpath"
    continue
  fi

  if is_excluded "$relpath" "$filename"; then
    echo "Skipping excluded: $relpath"
    continue
  fi

  load_run_case "$relpath"
  run_command=("${DOTNET_CMD[@]}" "$dll")
  if (( ${#RUN_ARGS[@]} > 0 )); then
    run_command+=("${RUN_ARGS[@]}")
  fi
  echo "Running: ${run_command[*]}"
  if "${run_command[@]}"; then
    rc=0
  else
    rc=$?
  fi

  if [[ "$rc" -eq "$EXPECTED_EXIT" ]]; then
    if [[ "$EXPECTED_EXIT" -eq 0 ]]; then
      echo "✅ Success: $relpath"
    else
      echo "✅ Success: $relpath (expected exit $EXPECTED_EXIT)"
    fi
    successes+=("$relpath")
  else
    echo "❌ Failed ($rc, expected $EXPECTED_EXIT): $relpath"
    failures+=("$relpath (exit $rc, expected $EXPECTED_EXIT)")
  fi
  echo
done

echo "===== Summary ====="
echo "Succeeded: ${#successes[@]}"
for s in "${successes[@]-}"; do echo "  - $s"; done
echo "Failed:    ${#failures[@]}"
if (( ${#failures[@]} > 0 )); then
  for f in "${failures[@]}"; do echo "  - $f"; done
fi

# Exit non-zero if any failed; still ran all of them.
(( ${#failures[@]} > 0 )) && exit 1 || exit 0
