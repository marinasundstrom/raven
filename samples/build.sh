#!/usr/bin/env bash
# Compile all .rav files in samples/ except an exclude list.
# Failing one won't stop the others.

set -Euo pipefail
shopt -s nullglob

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$SCRIPT_DIR"

PROJECT_DIR="$REPO_ROOT/src/Raven.Compiler"


RAVEN_CORE="${RAVEN_CORE:-$SCRIPT_DIR/Raven.Core.dll}"

if [[ ! -f "$RAVEN_CORE" ]]; then
  BUILD_RAVEN_CORE="$REPO_ROOT/src/Raven.Core/bin/Debug/net9.0/net9.0/Raven.Core.dll"
  if [[ -f "$BUILD_RAVEN_CORE" ]]; then
    RAVEN_CORE="$BUILD_RAVEN_CORE"
  fi
fi

# Resolve Raven.CodeAnalysis.dll
RAVEN_CODE_ANALYSIS="${RAVEN_CODE_ANALYSIS:-$SCRIPT_DIR/Raven.CodeAnalysis.dll}"

if [[ ! -f "$RAVEN_CODE_ANALYSIS" ]]; then
  BUILD_RAVEN_CODE_ANALYSIS="$REPO_ROOT/src/Raven.CodeAnalysis/bin/Debug/net9.0/Raven.CodeAnalysis.dll"
  BUILD_RAVEN_CODE_ANALYSIS_ALT="$REPO_ROOT/src/Raven.CodeAnalysis/bin/Debug/net9.0/net9.0/Raven.CodeAnalysis.dll"
  if [[ -f "$BUILD_RAVEN_CODE_ANALYSIS" ]]; then
    RAVEN_CODE_ANALYSIS="$BUILD_RAVEN_CODE_ANALYSIS"
  elif [[ -f "$BUILD_RAVEN_CODE_ANALYSIS_ALT" ]]; then
    RAVEN_CODE_ANALYSIS="$BUILD_RAVEN_CODE_ANALYSIS_ALT"
  fi
fi

OUTPUT_DIR="${OUTPUT_DIR:-output}"
if [[ "$OUTPUT_DIR" != /* ]]; then
  OUTPUT_DIR="$SCRIPT_DIR/$OUTPUT_DIR"
fi
if [[ -d "$OUTPUT_DIR" ]]; then
  rm -rf "$OUTPUT_DIR"
fi
mkdir -p "$OUTPUT_DIR"

DOTNET_VERSION="net9.0"
BUILD_CONFIG="Debug"
COMPILER_EXC="ravc"

# List of sample files (filenames only) to exclude
EXCLUDE=(
  "test.rav"
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

rav_files=( *.rav async/*.rav generics/*.rav discriminated-union/*.rav extensions/*.rav linq/*.rav oop/*.rav patterns/*.rav )

if (( ${#rav_files[@]} == 0 )); then
  echo "No .rav files found under samples/."
  exit 0
fi

#
# Make sure the compiler has been built
dotnet build "$PROJECT_DIR" -c "$BUILD_CONFIG"
dotnet build "$REPO_ROOT/src/Raven.CodeAnalysis" -c "$BUILD_CONFIG"
dotnet build "$REPO_ROOT/src/TestDep" -c "$BUILD_CONFIG"
COMPILER_BIN="$PROJECT_DIR/bin/$BUILD_CONFIG/$DOTNET_VERSION/$COMPILER_EXC"

failures=()
successes=()

if [[ ! -f "$RAVEN_CORE" ]]; then
  echo "Warning: Raven.Core.dll not found; samples will be built without --raven-core"
fi

for file in "${rav_files[@]}"; do
  filename=$(basename "$file")

  if is_excluded "$filename"; then
    echo "Skipping excluded: $filename"
    continue
  fi

  base="${filename%.rav}"
  output="$OUTPUT_DIR/$base.dll"

  echo "Compiling: $file -> $output"

  cmd=("$COMPILER_BIN" -- "$file" -o "$output")
  if [[ -f "$RAVEN_CORE" ]]; then
    cmd+=(--raven-core "$RAVEN_CORE")
  fi

  if ! "${cmd[@]}"; then
    rc=$?
    echo "❌ Compile failed ($rc): $filename"
    failures+=("$filename (exit $rc)")
  else
    echo "✅ Compile succeeded: $filename"
    successes+=("$filename")
  fi
  echo
done

# Copy dependency (this should not stop script if missing)
TEST_DEP_DLL="$REPO_ROOT/src/TestDep/bin/$BUILD_CONFIG/$DOTNET_VERSION/TestDep.dll"
cp "$TEST_DEP_DLL" "$OUTPUT_DIR"/TestDep.dll 2>/dev/null || \
  echo "Warning: Could not copy TestDep.dll"

if [[ -f "$RAVEN_CORE" ]]; then
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
echo "Succeeded: ${#successes[@]}"
for s in "${successes[@]}"; do echo "  - $s"; done
echo "Failed:    ${#failures[@]}"
for f in "${failures[@]}"; do echo "  - $f"; done

# Exit non-zero if any failed; still compiled all files.
(( ${#failures[@]} > 0 )) && exit 1 || exit 0
