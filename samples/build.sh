#!/usr/bin/env bash
# Compile all .rav/.rvn files in samples/ except an exclude list.
# Failing one won't stop the others.

set -Euo pipefail
shopt -s nullglob

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$SCRIPT_DIR"

PROJECT_DIR="$REPO_ROOT/src/Raven.Compiler"
DOTNET_VERSION="${DOTNET_VERSION:-net10.0}"
BUILD_CONFIG="${BUILD_CONFIG:-Debug}"
COMPILER_EXC="rvnc"

usage() {
  cat <<EOF
Usage: ./build.sh [options] [filter...]

Options:
  -f, --framework <tfm>   Target framework used for compiler/dependencies (default: ${DOTNET_VERSION})
  -c, --configuration <c> Build configuration (default: ${BUILD_CONFIG})
  -h, --help              Show this help

Filters:
  filter                  Optional path/name/glob filter(s) for sample selection.
                          Examples:
                            ./build.sh oop/enums.rav
                            ./build.sh async/*
                            ./build.sh -f net11.0 sample100.rav
                          If omitted, all samples are compiled.

Environment overrides:
  DOTNET_VERSION, BUILD_CONFIG, OUTPUT_DIR, RAVEN_CORE, RAVEN_CODE_ANALYSIS,
  EXCLUSIONS_FILE, CLEAN_OUTPUT, FORCE_REBUILD, BUILD_REPORT_TSV, BUILD_REPORT_MD
EOF
}

FILTERS=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    -f|--framework)
      [[ $# -lt 2 ]] && { echo "Missing value for $1"; exit 2; }
      DOTNET_VERSION="$2"
      shift 2
      ;;
    -c|--configuration)
      [[ $# -lt 2 ]] && { echo "Missing value for $1"; exit 2; }
      BUILD_CONFIG="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      FILTERS+=("$1")
      shift
      ;;
  esac
done

# Prefer a freshly built Raven.Core from this workspace.
# Do not fall back to a local samples copy because it can go stale.
if [[ -n "${RAVEN_CORE:-}" ]]; then
  RAVEN_CORE="$RAVEN_CORE"
fi

# Resolve Raven.CodeAnalysis.dll.
# Prefer the workspace build output for the selected TFM to avoid stale local copies.
RAVEN_CODE_ANALYSIS="${RAVEN_CODE_ANALYSIS:-}"

# Default output is split by TFM to avoid cross-framework artifact contamination.
OUTPUT_DIR="${OUTPUT_DIR:-output/$DOTNET_VERSION}"
if [[ "$OUTPUT_DIR" != /* ]]; then
  OUTPUT_DIR="$SCRIPT_DIR/$OUTPUT_DIR"
fi
CLEAN_OUTPUT="${CLEAN_OUTPUT:-0}"
FORCE_REBUILD="${FORCE_REBUILD:-0}"
if [[ "$CLEAN_OUTPUT" == "1" && -d "$OUTPUT_DIR" ]]; then
  rm -rf "$OUTPUT_DIR"
fi
mkdir -p "$OUTPUT_DIR"
BUILD_REPORT_TSV="${BUILD_REPORT_TSV:-$OUTPUT_DIR/build-report.tsv}"
BUILD_REPORT_MD="${BUILD_REPORT_MD:-$OUTPUT_DIR/build-report.md}"

timestamp_ms() {
  perl -MTime::HiRes=time -e 'printf "%d\n", time() * 1000'
}

format_duration_ms() {
  perl -e 'printf "%.3f", $ARGV[0] / 1000' "$1"
}

extract_compile_duration() {
  awk '
    /Build (succeeded|failed).* in / {
      line = $0
      sub(/^.* in /, "", line)
      sub(/\r$/, "", line)
      print line
    }
  ' "$1" | tail -n 1
}

escape_markdown_cell() {
  local value="$1"
  value="${value//|/\\|}"
  printf '%s' "$value"
}

tracked_source_file() {
  case "$1" in
    *.cs|*.csproj|*.props|*.targets|*.xml|*.rav|*.rvn|*.rvnproj|Directory.Build.*)
      return 0
      ;;
    *)
      return 1
      ;;
  esac
}

needs_rebuild() {
  local target="$1"
  shift

  if [[ "$FORCE_REBUILD" == "1" || ! -f "$target" ]]; then
    return 0
  fi

  local input
  for input in "$@"; do
    [[ -e "$input" ]] || continue

    if [[ -f "$input" ]]; then
      if [[ "$input" -nt "$target" ]]; then
        return 0
      fi
      continue
    fi

    while IFS= read -r candidate; do
      if tracked_source_file "$candidate" && [[ "$candidate" -nt "$target" ]]; then
        return 0
      fi
    done < <(find "$input" -type f)
  done

  return 1
}

EXCLUSIONS_FILE="${EXCLUSIONS_FILE:-$SCRIPT_DIR/exclusions.txt}"
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

    if [[ "$entry_mode" == "all" || "$entry_mode" == "$mode" ]]; then
      EXCLUDE_PATTERNS+=("$pattern")
    fi
  done < "$EXCLUSIONS_FILE"
}

is_excluded() {
  local path="$1"
  local filename="$2"
  local pattern
  for pattern in "${EXCLUDE_PATTERNS[@]-}"; do
    if [[ "$path" == $pattern || "$filename" == $pattern ]]; then
      return 0
    fi
  done
  return 1
}

matches_filter() {
  local path="$1"
  local filename="$2"

  if (( ${#FILTERS[@]} == 0 )); then
    return 0
  fi

  for filter in "${FILTERS[@]}"; do
    if [[ "$path" == $filter || "$filename" == $filter ]]; then
      return 0
    fi
  done

  return 1
}

load_exclusions "build"

sample_files=()

is_raven_source_file() {
  case "$1" in
    *.rav|*.rvn)
      return 0
      ;;
    *)
      return 1
      ;;
  esac
}

sample_output_stem() {
  local file="$1"
  case "$file" in
    *.rav|*.rvn)
      printf '%s\n' "${file%.*}"
      ;;
    *)
      printf '%s\n' "$file"
      ;;
  esac
}

collect_filtered_sample_files() {
  local filter resolved stripped matched=()

  for filter in "${FILTERS[@]}"; do
    if [[ "$filter" == *[\*\?\[]* ]]; then
      return 1
    fi

    if [[ "$filter" = /* ]]; then
      resolved="$filter"
    else
      resolved="$SCRIPT_DIR/$filter"
    fi

    if [[ ! -f "$resolved" ]] || ! is_raven_source_file "$resolved"; then
      return 1
    fi

    stripped="${resolved#$SCRIPT_DIR/}"
    if [[ "$stripped" == "$resolved" || "$stripped" == output/* || "$stripped" == projects/* ]]; then
      return 1
    fi

    matched+=("$stripped")
  done

  printf '%s\n' "${matched[@]}" | sort -u
}

if (( ${#FILTERS[@]} > 0 )); then
  while IFS= read -r file; do
    [[ -n "$file" ]] && sample_files+=("$file")
  done < <(collect_filtered_sample_files || find . -type f \( -name "*.rav" -o -name "*.rvn" \) ! -path "./output/*" ! -path "./projects/*" | sort | sed 's#^\./##')
else
  while IFS= read -r file; do
    sample_files+=("${file#./}")
  done < <(find . -type f \( -name "*.rav" -o -name "*.rvn" \) ! -path "./output/*" ! -path "./projects/*" | sort)
fi

if (( ${#sample_files[@]} == 0 )); then
  echo "No .rav/.rvn files found under samples/."
  exit 0
fi

if [[ -z "${RAVEN_CORE:-}" ]]; then
  DIRECT_RAVEN_CORE="$REPO_ROOT/src/Raven.Core/bin/$BUILD_CONFIG/$DOTNET_VERSION/Raven.Core.dll"
  if needs_rebuild \
    "$DIRECT_RAVEN_CORE" \
    "$REPO_ROOT/src/Raven.Core" \
    "$REPO_ROOT/src/Raven.Compiler" \
    "$REPO_ROOT/src/Raven.CodeAnalysis" \
    "$REPO_ROOT/src/Raven.CodeAnalysis.Console" \
    "$REPO_ROOT/src/RavenDoc"
  then
    rm -f "$DIRECT_RAVEN_CORE"
    dotnet build "$REPO_ROOT/src/Raven.Core" -c "$BUILD_CONFIG" -f "$DOTNET_VERSION"
  fi

  for candidate in \
    "$DIRECT_RAVEN_CORE" \
    "$REPO_ROOT/src/Raven.Core/bin/$BUILD_CONFIG/$DOTNET_VERSION/$DOTNET_VERSION/Raven.Core.dll"
  do
    if [[ -f "$candidate" ]]; then
      RAVEN_CORE="$candidate"
      break
    fi
  done
fi

COMPILER_BIN="$PROJECT_DIR/bin/$BUILD_CONFIG/$DOTNET_VERSION/$COMPILER_EXC"
RAVEN_CODE_ANALYSIS_BIN="$REPO_ROOT/src/Raven.CodeAnalysis/bin/$BUILD_CONFIG/$DOTNET_VERSION/Raven.CodeAnalysis.dll"
TEST_DEP_BIN="$REPO_ROOT/src/TestDep/bin/$BUILD_CONFIG/$DOTNET_VERSION/TestDep.dll"

#
# Rebuild only when outputs are missing or older than their inputs.
if needs_rebuild \
  "$RAVEN_CODE_ANALYSIS_BIN" \
  "$REPO_ROOT/src/Raven.CodeAnalysis" \
  "$REPO_ROOT/tools/NodeGenerator" \
  "$REPO_ROOT/tools/BoundNodeGenerator" \
  "$REPO_ROOT/tools/DiagnosticsGenerator"
then
  dotnet build "$REPO_ROOT/src/Raven.CodeAnalysis" -c "$BUILD_CONFIG" -f "$DOTNET_VERSION"
fi

if needs_rebuild \
  "$COMPILER_BIN" \
  "$PROJECT_DIR" \
  "$REPO_ROOT/src/Raven.CodeAnalysis" \
  "$REPO_ROOT/src/Raven.CodeAnalysis.Console" \
  "$REPO_ROOT/src/RavenDoc"
then
  dotnet build "$PROJECT_DIR" -c "$BUILD_CONFIG" -f "$DOTNET_VERSION" \
    -p:UseRavenCoreReference=false \
    -p:ProduceReferenceAssembly=true
fi

# TestDep is currently net10.0-only, so build it with its declared framework.
if needs_rebuild "$TEST_DEP_BIN" "$REPO_ROOT/src/TestDep"; then
  dotnet build "$REPO_ROOT/src/TestDep" -c "$BUILD_CONFIG"
fi

if [[ -z "${RAVEN_CORE:-}" || ! -f "$RAVEN_CORE" ]]; then
  DIRECT_RAVEN_CORE="$REPO_ROOT/src/Raven.Core/bin/$BUILD_CONFIG/$DOTNET_VERSION/Raven.Core.dll"
  RAVEN_CORE_SOURCES=()
  while IFS= read -r source; do
    RAVEN_CORE_SOURCES+=("$source")
  done < <(find "$REPO_ROOT/src/Raven.Core" -maxdepth 1 -name '*.rav' | sort)
  if [[ -x "$COMPILER_BIN" && ${#RAVEN_CORE_SOURCES[@]} -gt 0 ]]; then
    "$COMPILER_BIN" -- --emit-core-types-only --framework "$DOTNET_VERSION" --output-type classlib \
      -o "$DIRECT_RAVEN_CORE" "${RAVEN_CORE_SOURCES[@]}"
    if [[ -f "$DIRECT_RAVEN_CORE" ]]; then
      RAVEN_CORE="$DIRECT_RAVEN_CORE"
    fi
  fi
fi

if [[ -z "${RAVEN_CODE_ANALYSIS:-}" ]]; then
  for candidate in \
    "$REPO_ROOT/src/Raven.CodeAnalysis/bin/$BUILD_CONFIG/$DOTNET_VERSION/Raven.CodeAnalysis.dll" \
    "$REPO_ROOT/src/Raven.CodeAnalysis/bin/$BUILD_CONFIG/$DOTNET_VERSION/$DOTNET_VERSION/Raven.CodeAnalysis.dll" \
    "$SCRIPT_DIR/Raven.CodeAnalysis.dll"
  do
    if [[ -f "$candidate" ]]; then
      RAVEN_CODE_ANALYSIS="$candidate"
      break
    fi
  done
fi

failures=()
successes=()
sample_output_dirs=()
report_rows=()

if [[ -z "${RAVEN_CORE:-}" || ! -f "$RAVEN_CORE" ]]; then
  echo "Warning: Raven.Core.dll not found; samples will be built without --raven-core"
fi

for file in "${sample_files[@]}"; do
  filename=$(basename "$file")

  if is_excluded "$file" "$filename"; then
    echo "Skipping excluded: $file"
    continue
  fi

  if ! matches_filter "$file" "$filename"; then
    continue
  fi

  if [[ "$file" == */* ]]; then
    output="$OUTPUT_DIR/$(sample_output_stem "$file").dll"
  else
    # Keep root samples separated to avoid file/dir name collisions
    # (e.g. linq.rav vs linq/*.rav).
    output="$OUTPUT_DIR/_root/$(sample_output_stem "$file").dll"
  fi
  output_dir="$(dirname "$output")"
  mkdir -p "$output_dir"

  echo "Compiling: $file -> $output"

  cmd=("$COMPILER_BIN" -- "$file" -o "$output" --framework "$DOTNET_VERSION" --members-public-by-default)
  if [[ -n "${RAVEN_CORE:-}" && -f "$RAVEN_CORE" ]]; then
    cmd+=(--raven-core "$RAVEN_CORE")
  fi

  compile_log="$(mktemp "${TMPDIR:-/tmp}/raven-sample-build.XXXXXX")"
  started_ms="$(timestamp_ms)"
  set +e
  "${cmd[@]}" 2>&1 | tee "$compile_log"
  rc=${PIPESTATUS[0]}
  set -e
  ended_ms="$(timestamp_ms)"
  wall_ms=$((ended_ms - started_ms))
  compiler_duration="$(extract_compile_duration "$compile_log")"
  rm -f "$compile_log"
  if [[ -z "$compiler_duration" ]]; then
    compiler_duration="$(format_duration_ms "$wall_ms")s"
  fi

  if [[ "$rc" -eq 0 ]]; then
    echo "✅ Compile succeeded: $filename"
    successes+=("$file")
    sample_output_dirs+=("$output_dir")
    report_rows+=("$file"$'\t'"success"$'\t'"$compiler_duration"$'\t'"$wall_ms"$'\t'"0")
  else
    echo "❌ Compile failed ($rc): $filename"
    failures+=("$file (exit $rc)")
    report_rows+=("$file"$'\t'"failed"$'\t'"$compiler_duration"$'\t'"$wall_ms"$'\t'"$rc")
  fi
  echo
done

# Copy dependency (this should not stop script if missing)
TEST_DEP_DLL=""
for candidate in \
  "$TEST_DEP_BIN" \
  "$REPO_ROOT/src/TestDep/bin/$BUILD_CONFIG/net10.0/TestDep.dll"
do
  if [[ -f "$candidate" ]]; then
    TEST_DEP_DLL="$candidate"
    break
  fi
done
if [[ -z "$TEST_DEP_DLL" ]]; then
  echo "Warning: TestDep.dll not found"
fi

if [[ ! -f "$RAVEN_CODE_ANALYSIS" ]]; then
  echo "Warning: Raven.CodeAnalysis.dll not found; tried: $RAVEN_CODE_ANALYSIS"
fi

if (( ${#sample_output_dirs[@]} > 0 )); then
  while IFS= read -r dir; do
    [[ -z "$dir" ]] && continue
    if [[ -n "$TEST_DEP_DLL" ]]; then
      cp "$TEST_DEP_DLL" "$dir"/TestDep.dll 2>/dev/null || \
        echo "Warning: Could not copy TestDep.dll to $dir"
    fi

    if [[ -n "${RAVEN_CORE:-}" && -f "$RAVEN_CORE" ]]; then
      cp "$RAVEN_CORE" "$dir"/Raven.Core.dll 2>/dev/null || \
        echo "Warning: Could not copy Raven.Core.dll to $dir"
    fi

    if [[ -f "$RAVEN_CODE_ANALYSIS" ]]; then
      cp "$RAVEN_CODE_ANALYSIS" "$dir"/Raven.CodeAnalysis.dll 2>/dev/null || \
        echo "Warning: Could not copy Raven.CodeAnalysis.dll to $dir"
    fi
  done < <(printf '%s\n' "${sample_output_dirs[@]}" | sort -u)
fi

echo "===== Compile Summary ====="
if (( ${#FILTERS[@]} > 0 )); then
  echo "Filters:   ${FILTERS[*]}"
fi
if (( ${#EXCLUDE_PATTERNS[@]} > 0 )); then
  echo "Excludes:  ${EXCLUSIONS_FILE}"
fi
echo "Succeeded: ${#successes[@]}"
if (( ${#successes[@]} > 0 )); then
  for s in "${successes[@]}"; do echo "  - $s"; done
fi
echo "Failed:    ${#failures[@]}"
if (( ${#failures[@]} > 0 )); then
  for f in "${failures[@]}"; do echo "  - $f"; done
fi

mkdir -p "$(dirname "$BUILD_REPORT_TSV")" "$(dirname "$BUILD_REPORT_MD")"
{
  printf 'sample\tstatus\tcompiler_duration\twall_ms\twall_s\texit_code\n'
  if (( ${#report_rows[@]} > 0 )); then
    for row in "${report_rows[@]}"; do
      IFS=$'\t' read -r sample status compiler_duration wall_ms exit_code <<< "$row"
      printf '%s\t%s\t%s\t%s\t%s\t%s\n' \
        "$sample" \
        "$status" \
        "$compiler_duration" \
        "$wall_ms" \
        "$(format_duration_ms "$wall_ms")" \
        "$exit_code"
    done
  fi
} > "$BUILD_REPORT_TSV"

{
  printf '# Sample Build Report\n\n'
  printf -- '- Framework: `%s`\n' "$DOTNET_VERSION"
  printf -- '- Configuration: `%s`\n' "$BUILD_CONFIG"
  printf -- '- Output: `%s`\n\n' "$OUTPUT_DIR"
  printf '## Slowest Samples\n\n'
  printf '| Sample | Status | Compiler Time | Wall Time | Exit Code |\n'
  printf '| --- | --- | ---: | ---: | ---: |\n'
  if (( ${#report_rows[@]} > 0 )); then
    while IFS=$'\t' read -r sample status compiler_duration wall_ms exit_code; do
      [[ -z "${sample:-}" ]] && continue
      if [[ "$status" == "success" ]]; then
        status_icon="✅"
      else
        status_icon="❌"
      fi

      printf '| `%s` | %s %s | %s | %ss | %s |\n' \
        "$(escape_markdown_cell "$sample")" \
        "$status_icon" \
        "$status" \
        "$compiler_duration" \
        "$(format_duration_ms "$wall_ms")" \
        "$exit_code"
    done < <(printf '%s\n' "${report_rows[@]}" | sort -t $'\t' -k4,4nr | head -n 20)
  fi

  printf '\n## All Samples\n\n'
  printf '| Sample | Status | Compiler Time | Wall Time | Exit Code |\n'
  printf '| --- | --- | ---: | ---: | ---: |\n'
  if (( ${#report_rows[@]} > 0 )); then
    for row in "${report_rows[@]}"; do
      IFS=$'\t' read -r sample status compiler_duration wall_ms exit_code <<< "$row"
      if [[ "$status" == "success" ]]; then
        status_icon="✅"
      else
        status_icon="❌"
      fi

      printf '| `%s` | %s %s | %s | %ss | %s |\n' \
        "$(escape_markdown_cell "$sample")" \
        "$status_icon" \
        "$status" \
        "$compiler_duration" \
        "$(format_duration_ms "$wall_ms")" \
        "$exit_code"
    done
  fi
} > "$BUILD_REPORT_MD"

echo "Reports:"
echo "  - $BUILD_REPORT_TSV"
echo "  - $BUILD_REPORT_MD"

# Exit non-zero if any failed; still compiled all files.
(( ${#failures[@]} > 0 )) && exit 1 || exit 0
