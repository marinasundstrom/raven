#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

"$REPO_ROOT/scripts/codex-build.sh"
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
