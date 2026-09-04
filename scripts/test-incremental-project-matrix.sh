#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

projects=(
  "hello-world/HelloWorld.rvnproj"
  "conditional-compilation/ConditionalCompilation.rvnproj"
  "top-level-members/NamespaceMembers.rvnproj"
  "repository-result-patterns/RepositoryResultPatterns.rvnproj"
)

for project in "${projects[@]}"; do
  dotnet build \
    "$repo_root/samples/projects/$project" \
    --property WarningLevel=0
done

dotnet test \
  "$repo_root/test/Raven.LanguageServer.Integration.Tests/Raven.LanguageServer.Integration.Tests.csproj" \
  --filter 'FullyQualifiedName~SampleProjectMatrix_EditorEditsAndUndoRecoverAsync' \
  --logger 'console;verbosity=detailed' \
  /property:WarningLevel=0
