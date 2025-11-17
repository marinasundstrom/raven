#!/usr/bin/env bash
# Compile all .rav files in samples/ except an exclude list.
# Failing one won't stop the others.

set -Euo pipefail
shopt -s nullglob

OUTPUT_DIR="output"
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
  "tokenizer.rav"
  #"async-generic-compute.rav"
  # add others here if needed
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

rav_files=( samples/*.rav samples/async/*.rav samples/generics/*.rav )

if (( ${#rav_files[@]} == 0 )); then
  echo "No .rav files found under samples/."
  exit 0
fi

# Make sure the compiler has been built
dotnet build -c $BUILD_CONFIG

failures=()
successes=()

for file in "${rav_files[@]}"; do
  filename=$(basename "$file")

  if is_excluded "$filename"; then
    echo "Skipping excluded: $filename"
    continue
  fi

  base="${filename%.rav}"
  output="$OUTPUT_DIR/$base.dll"

  echo "Compiling: $file -> $output"
  
  if ! "./bin/$BUILD_CONFIG/$DOTNET_VERSION/$COMPILER_EXC" -- "$file" -o "$output"; then
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
cp ../TestDep/bin/$BUILD_CONFIG/$DOTNET_VERSION/TestDep.dll "$OUTPUT_DIR"/TestDep.dll 2>/dev/null || \
  echo "Warning: Could not copy TestDep.dll"

echo "===== Compile Summary ====="
echo "Succeeded: ${#successes[@]}"
for s in "${successes[@]}"; do echo "  - $s"; done
echo "Failed:    ${#failures[@]}"
for f in "${failures[@]}"; do echo "  - $f"; done

# Exit non-zero if any failed; still compiled all files.
(( ${#failures[@]} > 0 )) && exit 1 || exit 0
