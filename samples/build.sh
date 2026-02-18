#!/usr/bin/env bash
# Compile all .rav files in samples/ except an exclude list.
# Failing one won't stop the others.

set -Euo pipefail
shopt -s nullglob

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$SCRIPT_DIR"

PROJECT_DIR="$REPO_ROOT/src/Raven.Compiler"
DOTNET_VERSION="${DOTNET_VERSION:-net9.0}"
BUILD_CONFIG="${BUILD_CONFIG:-Debug}"
COMPILER_EXC="ravc"

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
  DOTNET_VERSION, BUILD_CONFIG, OUTPUT_DIR, RAVEN_CORE, RAVEN_CODE_ANALYSIS
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
if [[ -d "$OUTPUT_DIR" ]]; then
  rm -rf "$OUTPUT_DIR"
fi
mkdir -p "$OUTPUT_DIR"

# List of sample files (filenames only) to exclude
EXCLUDE=(
  "test.rav"
  "args-to-generic-methods.rav"
)

is_excluded() {
  local file="$1"
  for ex in "${EXCLUDE[@]}"; do
    if [[ "$file" == "$ex" ]]; then
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

rav_files=( *.rav async/*.rav generics/*.rav discriminated-union/*.rav extensions/*.rav linq/*.rav oop/*.rav patterns/*.rav result-and-options/*.rav sandbox/*.rav unmanaged/*.rav )

if (( ${#rav_files[@]} == 0 )); then
  echo "No .rav files found under samples/."
  exit 0
fi

#
# Make sure the compiler has been built
dotnet build "$PROJECT_DIR" -c "$BUILD_CONFIG" -f "$DOTNET_VERSION"
dotnet build "$REPO_ROOT/src/Raven.CodeAnalysis" -c "$BUILD_CONFIG" -f "$DOTNET_VERSION"
# TestDep is currently net9.0-only, so build it with its declared framework.
dotnet build "$REPO_ROOT/src/TestDep" -c "$BUILD_CONFIG"

if [[ -z "${RAVEN_CORE:-}" ]]; then
  rm -f "$REPO_ROOT/src/Raven.Core/bin/$BUILD_CONFIG/$DOTNET_VERSION/Raven.Core.dll"
  dotnet build "$REPO_ROOT/src/Raven.Core" -c "$BUILD_CONFIG" -f "$DOTNET_VERSION"

  for candidate in \
    "$REPO_ROOT/src/Raven.Core/bin/$BUILD_CONFIG/$DOTNET_VERSION/Raven.Core.dll" \
    "$REPO_ROOT/src/Raven.Core/bin/$BUILD_CONFIG/$DOTNET_VERSION/$DOTNET_VERSION/Raven.Core.dll"
  do
    if [[ -f "$candidate" ]]; then
      RAVEN_CORE="$candidate"
      break
    fi
  done
fi

COMPILER_BIN="$PROJECT_DIR/bin/$BUILD_CONFIG/$DOTNET_VERSION/$COMPILER_EXC"

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

if [[ -z "${RAVEN_CORE:-}" || ! -f "$RAVEN_CORE" ]]; then
  echo "Warning: Raven.Core.dll not found; samples will be built without --raven-core"
fi

for file in "${rav_files[@]}"; do
  filename=$(basename "$file")

  if is_excluded "$filename"; then
    echo "Skipping excluded: $filename"
    continue
  fi

  if ! matches_filter "$file" "$filename"; then
    continue
  fi

  base="${filename%.rav}"
  output="$OUTPUT_DIR/$base.dll"

  echo "Compiling: $file -> $output"

  cmd=("$COMPILER_BIN" -- "$file" -o "$output" --framework "$DOTNET_VERSION")
  if [[ -n "${RAVEN_CORE:-}" && -f "$RAVEN_CORE" ]]; then
    cmd+=(--raven-core "$RAVEN_CORE")
  fi

  if "${cmd[@]}"; then
    echo "✅ Compile succeeded: $filename"
    successes+=("$filename")
  else
    rc=$?
    echo "❌ Compile failed ($rc): $filename"
    failures+=("$filename (exit $rc)")
  fi
  echo
done

# Copy dependency (this should not stop script if missing)
TEST_DEP_DLL=""
for candidate in \
  "$REPO_ROOT/src/TestDep/bin/$BUILD_CONFIG/$DOTNET_VERSION/TestDep.dll" \
  "$REPO_ROOT/src/TestDep/bin/$BUILD_CONFIG/net9.0/TestDep.dll"
do
  if [[ -f "$candidate" ]]; then
    TEST_DEP_DLL="$candidate"
    break
  fi
done
if [[ -n "$TEST_DEP_DLL" ]]; then
  cp "$TEST_DEP_DLL" "$OUTPUT_DIR"/TestDep.dll 2>/dev/null || \
    echo "Warning: Could not copy TestDep.dll"
else
  echo "Warning: TestDep.dll not found"
fi

if [[ -n "${RAVEN_CORE:-}" && -f "$RAVEN_CORE" ]]; then
  cp "$RAVEN_CORE" "$OUTPUT_DIR"/ 2>/dev/null || \
    echo "Warning: Could not copy Raven.Core.dll"
fi


if [[ -f "$RAVEN_CODE_ANALYSIS" ]]; then
  cp "$RAVEN_CODE_ANALYSIS" "$OUTPUT_DIR"/Raven.CodeAnalysis.dll 2>/dev/null || \
    echo "Warning: Could not copy Raven.CodeAnalysis.dll"
else
  echo "Warning: Raven.CodeAnalysis.dll not found; tried: $RAVEN_CODE_ANALYSIS"
fi

echo "===== Compile Summary ====="
if (( ${#FILTERS[@]} > 0 )); then
  echo "Filters:   ${FILTERS[*]}"
fi
echo "Succeeded: ${#successes[@]}"
for s in "${successes[@]-}"; do echo "  - $s"; done
echo "Failed:    ${#failures[@]}"
for f in "${failures[@]-}"; do echo "  - $f"; done

# Exit non-zero if any failed; still compiled all files.
(( ${#failures[@]} > 0 )) && exit 1 || exit 0
