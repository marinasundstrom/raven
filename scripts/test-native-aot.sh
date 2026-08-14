#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
temporary_root="${TMPDIR:-/tmp}"
output_directory="$(mktemp -d "$temporary_root/raven-native-aot-smoke.XXXXXX")"
log_file="$(mktemp "$temporary_root/raven-native-aot-smoke.XXXXXX.log")"
ilverify_directory="$(mktemp -d "$temporary_root/raven-native-aot-ilverify.XXXXXX")"
core_output_directory="$(mktemp -d "$temporary_root/raven-core-release-smoke.XXXXXX")"
package_cache_directory="$(mktemp -d "$temporary_root/raven-native-aot-packages.XXXXXX")"

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

  case "$core_output_directory" in
    "$temporary_root"/raven-core-release-smoke.*)
      if [[ -d "$core_output_directory" && ! -L "$core_output_directory" ]]; then
        rm -rf -- "$core_output_directory"
      fi
      ;;
  esac

  case "$package_cache_directory" in
    "$temporary_root"/raven-native-aot-packages.*)
      if [[ -d "$package_cache_directory" && ! -L "$package_cache_directory" ]]; then
        rm -rf -- "$package_cache_directory"
      fi
      ;;
  esac
}
trap cleanup EXIT

compiler="$repo_root/src/Raven.Compiler/bin/Debug/net11.0/rvnc.dll"
project="$repo_root/samples/projects/greenhouse-monitor/GreenhouseMonitor.rvnproj"
sdk_version="$(sed -n 's/.*"Raven.Sdk"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p' "$repo_root/global.json")"

if [[ -z "$sdk_version" ]]; then
  echo "Could not determine the centrally selected Raven.Sdk version from global.json." >&2
  exit 1
fi

if [[ "${FORCE_REBUILD:-0}" == "1" || ! -f "$compiler" ]]; then
  "$repo_root/scripts/codex-build.sh"
fi

# Samples use the same NuGet-resolved Raven.Sdk contract as external projects.
# Build the official lockstep package family into the repository-local feed so
# a clean source checkout exercises normal MSBuild SDK resolution as well.
if [[ ! -f "$repo_root/artifacts/packages/Raven.Sdk.$sdk_version.nupkg" ]]; then
  RAVEN_SKIP_PACKAGE_VALIDATION=1 \
    "$repo_root/scripts/package-nuget.sh" "$sdk_version"
fi

# The selected version may already exist on NuGet.org. Restore the packages we
# just built into an isolated cache so this test cannot reuse an older package
# with the same version from the machine-wide cache.
export NUGET_PACKAGES="$package_cache_directory"

dotnet tool restore --tool-manifest "$repo_root/.config/dotnet-tools.json"

dotnet "$compiler" \
  "$repo_root/src/Raven.Core/Raven.Core.rvnproj" \
  --configuration Release \
  --framework net10.0 \
  --output-type classlib \
  --emit-core-types-only \
  --output "$core_output_directory"

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
