#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RAVEN_COMPILER_PROJECT="$ROOT_DIR/src/Raven.Compiler/Raven.Compiler.csproj"

assert_file_exists() {
  local file_path="$1"
  if [[ ! -f "$file_path" ]]; then
    echo "Missing expected file: $file_path" >&2
    exit 1
  fi
}

read_project_target_framework() {
  local project_file="$1"
  local tfm
  tfm="$(sed -nE 's/.*TargetFramework=\"([^\"]+)\".*/\1/p' "$project_file" | head -n1)"
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

aspnet_project="$ROOT_DIR/samples/projects/aspnet-minimal-api/AspNetMinimalApi.ravenproj"
aspnet_tfm="$(read_project_target_framework "$aspnet_project")"
aspnet_output_dir="$ROOT_DIR/samples/projects/aspnet-minimal-api/bin/Debug"
mkdir -p "$aspnet_output_dir"
compile_project_for_debug "$aspnet_project" "$aspnet_output_dir" "$aspnet_tfm"
assert_file_exists "$aspnet_output_dir/AspNetMinimalApi.dll"
assert_file_exists "$aspnet_output_dir/AspNetMinimalApi.pdb"
assert_file_exists "$aspnet_output_dir/AspNetMinimalApi.runtimeconfig.json"
assert_file_exists "$aspnet_output_dir/Microsoft.AspNetCore.dll"

efcore_project="$ROOT_DIR/samples/projects/efcore-expression-trees/EfCoreExpressionTrees.ravenproj"
efcore_tfm="$(read_project_target_framework "$efcore_project")"
efcore_output_dir="$ROOT_DIR/samples/projects/efcore-expression-trees/bin/Debug"
mkdir -p "$efcore_output_dir"
compile_project_for_debug "$efcore_project" "$efcore_output_dir" "$efcore_tfm"
assert_file_exists "$efcore_output_dir/EfCoreExpressionTrees.dll"
assert_file_exists "$efcore_output_dir/EfCoreExpressionTrees.pdb"
assert_file_exists "$efcore_output_dir/EfCoreExpressionTrees.runtimeconfig.json"
assert_file_exists "$efcore_output_dir/Microsoft.EntityFrameworkCore.dll"
assert_file_exists "$efcore_output_dir/Microsoft.EntityFrameworkCore.InMemory.dll"

echo "Debug sample smoke checks passed."

echo
echo "Running debug breakpoint binding checks..."
bash "$ROOT_DIR/scripts/test-debug-breakpoint-bindings.sh"
