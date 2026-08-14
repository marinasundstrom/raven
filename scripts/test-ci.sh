#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SDK_VERSION="$(sed -n 's/.*"Raven.Sdk"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p' "$REPO_ROOT/global.json")"

if [[ -z "$SDK_VERSION" ]]; then
  echo "Could not determine the centrally selected Raven.Sdk version from global.json." >&2
  exit 1
fi

"$REPO_ROOT/scripts/codex-build.sh"

# Workspace tests open repository samples through MSBuild. Build a Debug SDK
# package into the repository-local feed so a clean CI runner resolves the same
# compiler that was just built instead of depending on a previously published
# or globally cached package.
dotnet pack "$REPO_ROOT/sdk/Raven.Sdk/Raven.Sdk.csproj" \
  -c Debug \
  -o "$REPO_ROOT/artifacts/packages" \
  /property:RavenCompilerConfiguration=Debug \
  /property:Version="$SDK_VERSION" \
  /property:PackageVersion="$SDK_VERSION" \
  /property:InformationalVersion="$SDK_VERSION" \
  /property:IncludeSourceRevisionInInformationalVersion=false

"$REPO_ROOT/scripts/test-baseline.sh"
"$REPO_ROOT/scripts/test-runtime-isolated.sh"

language_server_projects=(
  "$REPO_ROOT/test/Raven.LanguageServer.Tests/Raven.LanguageServer.Tests.csproj"
  "$REPO_ROOT/test/Raven.LanguageServer.Integration.Tests/Raven.LanguageServer.Integration.Tests.csproj"
  "$REPO_ROOT/test/Raven.LanguageServer.Perf.Tests/Raven.LanguageServer.Perf.Tests.csproj"
)

for project in "${language_server_projects[@]}"; do
  dotnet test "$project" /property:WarningLevel=0
done
