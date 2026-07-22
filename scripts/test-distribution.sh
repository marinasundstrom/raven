#!/usr/bin/env bash
set -euo pipefail

SDK_ROOT="${1:-}"
if [[ -z "$SDK_ROOT" ]]; then
  echo "Usage: scripts/test-distribution.sh <sdk-root>" >&2
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

if [[ -x "$SDK_ROOT/bin/rvn" ]]; then
  actual_root="$("$SDK_ROOT/bin/rvn" sdk path)"
elif [[ -f "$SDK_ROOT/bin/rvn.cmd" ]]; then
  actual_root="$(dotnet "$SDK_ROOT/tools/rvn/rvn.dll" sdk path)"
else
  echo "Missing rvn launcher." >&2
  exit 1
fi

if [[ "$actual_root" != "$SDK_ROOT" ]]; then
  echo "rvn sdk path returned '$actual_root'; expected '$SDK_ROOT'." >&2
  exit 1
fi

echo "Validated Raven SDK: $SDK_ROOT"
