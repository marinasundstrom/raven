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

SDK_ROOT="$(cd "$SDK_ROOT" && pwd)"

required_files=(
  "VERSION"
  "sdk/Raven.Core.dll"
  "sdk/build/Raven.Language.targets"
  "sdk/build/Raven.MSBuild.props"
  "sdk/build/Raven.MSBuild.targets"
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

echo "Validated Raven SDK: $SDK_ROOT"
