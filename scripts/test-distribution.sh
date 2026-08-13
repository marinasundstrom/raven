#!/usr/bin/env bash
set -euo pipefail

STRUCTURE_ONLY=false
if [[ "${1:-}" == "--structure-only" ]]; then
  STRUCTURE_ONLY=true
  shift
fi

SDK_ROOT="${1:-}"
if [[ -z "$SDK_ROOT" ]]; then
  echo "Usage: scripts/test-distribution.sh [--structure-only] <sdk-root>" >&2
  exit 1
fi

SDK_ROOT="$(cd "$SDK_ROOT" && pwd -P)"

required_files=(
  "VERSION"
  "sdk/Raven.Core.dll"
  "sdk/Raven.Macros.dll"
  "sdk/build/Raven.Language.targets"
  "sdk/build/Raven.MSBuild.props"
  "sdk/build/Raven.nanoFramework.props"
  "sdk/build/Raven.nanoFramework.targets"
  "tools/rvn/rvn.dll"
  "tools/rvnc/rvnc.dll"
  "tools/language-server/Raven.LanguageServer.dll"
)

for relative_path in "${required_files[@]}"; do
  if [[ ! -f "$SDK_ROOT/$relative_path" ]]; then
    echo "Missing SDK file: $relative_path" >&2
    exit 1
  fi
done

SDK_VERSION="$(tr -d '\r\n' < "$SDK_ROOT/VERSION")"

if [[ "$STRUCTURE_ONLY" == true ]]; then
  echo "Validated Raven SDK structure: $SDK_ROOT"
  exit 0
fi

if [[ -x "$SDK_ROOT/bin/rvn" ]]; then
  actual_root="$("$SDK_ROOT/bin/rvn" sdk path)"
elif [[ -f "$SDK_ROOT/bin/rvn.cmd" ]]; then
  echo "Windows SDK execution requires a Windows host. Re-run with --structure-only on this platform." >&2
  exit 1
else
  echo "Missing rvn launcher." >&2
  exit 1
fi

if [[ "$actual_root" != "$SDK_ROOT" ]]; then
  echo "rvn sdk path returned '$actual_root'; expected '$SDK_ROOT'." >&2
  exit 1
fi

"$SDK_ROOT/bin/rvn" doctor

PROJECT_DIR="$(mktemp -d)"
if [[ -z "$PROJECT_DIR" || ! -d "$PROJECT_DIR" ]]; then
  echo "Failed to create a temporary project directory." >&2
  exit 1
fi

(
  cd "$PROJECT_DIR"
  "$SDK_ROOT/bin/rvn" init --list | grep -F 'web'
  "$SDK_ROOT/bin/rvn" init --list | grep -F 'nano'
  "$SDK_ROOT/bin/rvn" init --name InstalledProject
  grep -F "<Project Sdk=\"Raven.Sdk/$SDK_VERSION\">" InstalledProject.rvnproj
  grep -F '<TargetFramework>net11.0</TargetFramework>' InstalledProject.rvnproj
  grep -F 'func Main()' src/Main.rvn

  # The pre-publication archive smoke test runs before Raven.Sdk exists on the
  # public feed. Use the archive's packaged targets to validate rvn build/run;
  # the post-publication installation workflow validates the generated
  # Raven.Sdk project unchanged through the normal .NET CLI.
  perl -pi -e 's#Raven\.Sdk/[^"<]+#Microsoft.NET.Sdk#' InstalledProject.rvnproj
  "$SDK_ROOT/bin/rvn" build InstalledProject.rvnproj
  project_output="$("$SDK_ROOT/bin/rvn" run InstalledProject.rvnproj)"
  printf '%s\n' "$project_output"
  grep -F 'Hello from Raven' <<< "$project_output"

  mkdir classlib web nano
  (cd classlib && "$SDK_ROOT/bin/rvn" init classlib --name InstalledLibrary)
  (cd web && "$SDK_ROOT/bin/rvn" init web --name InstalledWeb)
  (cd nano && "$SDK_ROOT/bin/rvn" init nano --name InstalledNano)
  test -f classlib/src/Library.rvn
  grep -F '<FrameworkReference Include="Microsoft.AspNetCore.App" />' web/InstalledWeb.rvnproj
  grep -F '<TargetFramework>netnano1.0</TargetFramework>' nano/InstalledNano.rvnproj
)

echo "Validated Raven SDK: $SDK_ROOT"
