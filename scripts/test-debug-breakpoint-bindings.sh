#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RAVEN_COMPILER_PROJECT="$ROOT_DIR/src/Raven.Compiler/Raven.Compiler.csproj"
RAVEN_COMPILER_HOST="$ROOT_DIR/src/Raven.Compiler/bin/Debug/net11.0/rvnc.dll"
RAVEN_LANGUAGE_TARGETS="$ROOT_DIR/build/Raven.Language.targets"

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

compile_project_for_debug() {
  local project_file="$1"
  local output_dir="$2"

  mkdir -p "$output_dir"

  echo "Compiling $(basename "$project_file") for debugger coverage at '$output_dir'"
  dotnet build "$project_file" \
    --configuration Debug \
    --output "$output_dir" \
    --property WarningLevel=0 \
    "/property:BaseIntermediateOutputPath=$output_dir/obj/" \
    "/property:RavenCompilerHost=$RAVEN_COMPILER_HOST" \
    "/property:LanguageTargets=$RAVEN_LANGUAGE_TARGETS"
}

resolve_netcoredbg() {
  if [[ -n "${NETCOREDBG_PATH:-}" ]]; then
    if [[ ! -x "$NETCOREDBG_PATH" ]]; then
      echo "NETCOREDBG_PATH is not executable: $NETCOREDBG_PATH" >&2
      exit 1
    fi

    echo "$NETCOREDBG_PATH"
    return
  fi

  if command -v netcoredbg >/dev/null 2>&1; then
    command -v netcoredbg
    return
  fi

  echo "netcoredbg was not found. Install it or set NETCOREDBG_PATH to run breakpoint-hit coverage." >&2
  exit 1
}

run_breakpoint_hit_test() {
  local debugger_path="$1"
  local assembly_path="$2"
  local source_path="$3"
  local output_path="$4"
  local dotnet_host
  dotnet_host="$(command -v dotnet)"

  (
    cd "$(dirname "$output_path")"
    printf '%s\n' \
      "file $dotnet_host" \
      "set args $assembly_path" \
      "break $source_path:15" \
      "break $source_path:9" \
      "run" \
      "wait" \
      "bt" \
      "continue" \
      "wait" \
      "bt" \
      "continue" \
      "wait" \
      "quit" | "$debugger_path" --interpreter=cli > "$output_path"
  )

  assert_contains "$output_path" "NamespaceMembers.Main.*$source_path:15" "Debugger should hit the top-level Main breakpoint"
  assert_contains "$output_path" "Worker.AddOne.*$source_path:9" "Debugger should hit the class-method breakpoint after an abstract method"
}

require_command dotnet
require_command rg

TMP_DIR="$ROOT_DIR/.tmp/debug-breakpoint-bindings"
rm -rf "$TMP_DIR"
mkdir -p "$TMP_DIR"

# Build the current compiler, then execute a fixture under a managed debugger and
# prove source breakpoints are hit. PDB-shape coverage belongs in the unit tests;
# this script deliberately covers the user-visible debugger behavior.
dotnet build "$RAVEN_COMPILER_PROJECT" \
  --framework net11.0 \
  --property UseRavenCoreReference=false \
  --property WarningLevel=0

# The abstract interface method is intentional: persisted Reflection.Emit omits
# its MethodDebugInformation row, which previously shifted all later methods.
debugger_project="$ROOT_DIR/test/fixtures/debugger-breakpoints/DebuggerBreakpoints.rvnproj"
debugger_output_dir="$TMP_DIR/debugger-breakpoints"
compile_project_for_debug "$debugger_project" "$debugger_output_dir"
assert_file_exists "$debugger_output_dir/DebuggerBreakpoints.dll"
assert_file_exists "$debugger_output_dir/DebuggerBreakpoints.pdb"
debugger_log="$debugger_output_dir/debugger.log"
run_breakpoint_hit_test \
  "$(resolve_netcoredbg)" \
  "$debugger_output_dir/DebuggerBreakpoints.dll" \
  "$ROOT_DIR/test/fixtures/debugger-breakpoints/src/Main.rvn" \
  "$debugger_log"

echo "Debug breakpoint binding checks passed."
