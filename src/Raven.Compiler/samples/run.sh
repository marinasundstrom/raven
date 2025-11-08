#!/usr/bin/env bash
# Run all DLLs in ./output except an exclude list.
# Failing one won't stop the rest.

set -Euo pipefail
shopt -s nullglob

OUTPUT_DIR="${OUTPUT_DIR:-output}"

# List of dlls to exclude (filenames only)
EXCLUDE=(
  "TestDep.dll"
  "goto.dll"
  "parse-number.dll"
  # add others as needed
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

if [[ ! -d "$OUTPUT_DIR" ]]; then
  echo "Output directory '$OUTPUT_DIR' does not exist."
  exit 1
fi

dlls=( "$OUTPUT_DIR"/*.dll )

if (( ${#dlls[@]} == 0 )); then
  echo "No .dll files found in '$OUTPUT_DIR'."
  exit 0
fi

failures=()
successes=()

for dll in "${dlls[@]}"; do
  filename="$(basename "$dll")"

  if is_excluded "$filename"; then
    echo "Skipping excluded: $filename"
    continue
  fi

  echo "Running: dotnet \"$dll\""
  if ! dotnet "$dll"; then
    rc=$?
    echo "❌ Failed ($rc): $filename"
    failures+=("$filename (exit $rc)")
  else
    echo "✅ Success: $filename"
    successes+=("$filename")
  fi
  echo
done

echo "===== Summary ====="
echo "Succeeded: ${#successes[@]}"
for s in "${successes[@]}"; do echo "  - $s"; done
echo "Failed:    ${#failures[@]}"
for f in "${failures[@]}"; do echo "  - $f"; done

# Exit non-zero if any failed; still ran all of them.
(( ${#failures[@]} > 0 )) && exit 1 || exit 0