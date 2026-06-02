#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PROJECT="$ROOT_DIR/test/Raven.LanguageServer.Perf.Tests/Raven.LanguageServer.Perf.Tests.csproj"

dotnet test "$PROJECT" /property:WarningLevel=0 --blame-hang-timeout 300s --blame-hang-dump-type none "$@"
