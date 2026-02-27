#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RAVEN_COMPILER_PROJECT="$ROOT_DIR/src/Raven.Compiler/Raven.Compiler.csproj"

require_command() {
  local cmd="$1"
  if ! command -v "$cmd" >/dev/null 2>&1; then
    echo "Required command '$cmd' was not found in PATH." >&2
    exit 1
  fi
}

assert_file_exists() {
  local file_path="$1"
  if [[ ! -f "$file_path" ]]; then
    echo "Missing expected file: $file_path" >&2
    exit 1
  fi
}

assert_contains() {
  local file_path="$1"
  local pattern="$2"
  local message="$3"
  if ! rg -q "$pattern" "$file_path"; then
    echo "Assertion failed: $message" >&2
    echo "Pattern: $pattern" >&2
    echo "File: $file_path" >&2
    exit 1
  fi
}

read_project_target_framework() {
  local project_file="$1"
  local tfm
  tfm="$(sed -nE 's/.*TargetFramework="([^"]+)".*/\1/p' "$project_file" | head -n1)"
  if [[ -z "$tfm" ]]; then
    echo "Could not read TargetFramework from '$project_file'." >&2
    exit 1
  fi

  echo "$tfm"
}

compile_project_for_debug() {
  local project_file="$1"
  local output_dir="$2"
  local tfm="$3"

  mkdir -p "$output_dir"

  echo "Compiling $(basename "$project_file") for debug output at '$output_dir' (TFM: $tfm)"
  dotnet run \
    --framework "$tfm" \
    --project "$RAVEN_COMPILER_PROJECT" \
    --property WarningLevel=0 \
    -- \
    "$project_file" \
    --publish \
    -o "$output_dir" \
    --framework "$tfm"
}

emit_sequence_listing() {
  local assembly_path="$1"
  local output_path="$2"
  ilspycmd --il-sequence-points --use-varnames-from-pdb "$assembly_path" > "$output_path"
}

validate_main_sequence_points() {
  local sequence_file="$1"
  local source_hint="$2"
  local label="$3"

  assert_contains "$sequence_file" "end of method Program::Main" "$label should emit Program::Main"
  assert_contains "$sequence_file" "sequence point:.*$source_hint" "$label should emit sequence points for '$source_hint'"
}

require_command dotnet
require_command ilspycmd
require_command rg

TMP_DIR="$ROOT_DIR/.tmp/debug-breakpoint-bindings"
rm -rf "$TMP_DIR"
mkdir -p "$TMP_DIR"

# Case 1: explicit top-level func Main
hello_project="$ROOT_DIR/samples/projects/hello-world/HelloWorld.ravenproj"
hello_tfm="$(read_project_target_framework "$hello_project")"
hello_output_dir="$TMP_DIR/hello-world"
compile_project_for_debug "$hello_project" "$hello_output_dir" "$hello_tfm"
assert_file_exists "$hello_output_dir/HelloWorld.dll"
assert_file_exists "$hello_output_dir/HelloWorld.pdb"
hello_seq="$hello_output_dir/HelloWorld.seq.il.txt"
emit_sequence_listing "$hello_output_dir/HelloWorld.dll" "$hello_seq"
validate_main_sequence_points "$hello_seq" "hello-world/src/main.rav" "HelloWorld"

# Case 2: implicit top-level statements (no explicit Main declaration)
nuget_project="$ROOT_DIR/samples/projects/nuget-demo/NuGetDemo.ravenproj"
nuget_tfm="$(read_project_target_framework "$nuget_project")"
nuget_output_dir="$TMP_DIR/nuget-demo"
compile_project_for_debug "$nuget_project" "$nuget_output_dir" "$nuget_tfm"
assert_file_exists "$nuget_output_dir/NuGetDemo.dll"
assert_file_exists "$nuget_output_dir/NuGetDemo.pdb"
nuget_seq="$nuget_output_dir/NuGetDemo.seq.il.txt"
emit_sequence_listing "$nuget_output_dir/NuGetDemo.dll" "$nuget_seq"
validate_main_sequence_points "$nuget_seq" "nuget-demo/src/main.rav" "NuGetDemo"

# Case 3: async Main with synthesized entrypoint wrapper
runtime_async_project="$ROOT_DIR/samples/projects/runtime-async-net11/RuntimeAsyncNet11.ravenproj"
runtime_async_tfm="$(read_project_target_framework "$runtime_async_project")"
runtime_async_output_dir="$TMP_DIR/runtime-async-net11"
compile_project_for_debug "$runtime_async_project" "$runtime_async_output_dir" "$runtime_async_tfm"
assert_file_exists "$runtime_async_output_dir/RuntimeAsyncNet11.dll"
assert_file_exists "$runtime_async_output_dir/RuntimeAsyncNet11.pdb"
runtime_async_seq="$runtime_async_output_dir/RuntimeAsyncNet11.seq.il.txt"
emit_sequence_listing "$runtime_async_output_dir/RuntimeAsyncNet11.dll" "$runtime_async_seq"
validate_main_sequence_points "$runtime_async_seq" "runtime-async-net11/src/main.rav" "RuntimeAsyncNet11"
assert_contains "$runtime_async_seq" "end of method Program::'<Main>_EntryPoint'" "RuntimeAsyncNet11 should emit synthesized async entrypoint wrapper"
assert_contains "$runtime_async_seq" "\.entrypoint" "RuntimeAsyncNet11 should mark entrypoint method"

echo "Debug breakpoint binding checks passed."
