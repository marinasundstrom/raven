#!/usr/bin/env bash
set -euo pipefail

REPOSITORY_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CONFIGURATION="${RAVEN_CONFIGURATION:-Debug}"
FRAMEWORK="${RAVEN_FRAMEWORK:-net11.0}"
COMPILER_HOST="$REPOSITORY_ROOT/src/Raven.Compiler/bin/$CONFIGURATION/$FRAMEWORK/rvnc.dll"

case "$FRAMEWORK" in
  net10.0|net11.0) ;;
  *)
    echo "Unsupported Raven development tool framework: $FRAMEWORK" >&2
    echo "Use net10.0 or net11.0 through RAVEN_FRAMEWORK." >&2
    exit 2
    ;;
esac

echo "==> Generating compiler sources"
"$REPOSITORY_ROOT/scripts/generate-compiler-sources.sh"

echo "==> Building bootstrap compiler ($CONFIGURATION, $FRAMEWORK)"
dotnet build "$REPOSITORY_ROOT/src/Raven.Compiler/Raven.Compiler.csproj" \
  --configuration "$CONFIGURATION" \
  --framework "$FRAMEWORK" \
  /property:UseRavenCoreReference=false \
  /property:WarningLevel=0

echo "==> Building Raven.Core ($CONFIGURATION, $FRAMEWORK)"
dotnet build "$REPOSITORY_ROOT/src/Raven.Core/Raven.Core.rvnproj" \
  --configuration "$CONFIGURATION" \
  --framework "$FRAMEWORK" \
  /property:RavenCompilerHost="$COMPILER_HOST" \
  /property:WarningLevel=0

echo "==> Building Raven.Macros ($CONFIGURATION, $FRAMEWORK)"
dotnet build "$REPOSITORY_ROOT/src/Raven.Macros/Raven.Macros.rvnproj" \
  --configuration "$CONFIGURATION" \
  --framework "$FRAMEWORK" \
  /property:BuildProjectReferences=false \
  /property:RavenCompilerHost="$COMPILER_HOST" \
  /property:WarningLevel=0

echo "==> Building repository command-line tools"
dotnet build "$REPOSITORY_ROOT/src/Raven.Compiler/Raven.Compiler.csproj" \
  --configuration "$CONFIGURATION" \
  --framework "$FRAMEWORK" \
  /property:WarningLevel=0
dotnet build "$REPOSITORY_ROOT/src/Raven/Raven.csproj" \
  --configuration "$CONFIGURATION" \
  --framework "$FRAMEWORK" \
  /property:WarningLevel=0

echo "==> Building repository language server"
dotnet build "$REPOSITORY_ROOT/src/Raven.LanguageServer/Raven.LanguageServer.csproj" \
  --configuration "$CONFIGURATION" \
  --framework "$FRAMEWORK" \
  /property:WarningLevel=0

echo "==> Building repository VS Code extension"
npm --prefix "$REPOSITORY_ROOT/src/Raven.VSCode" ci
npm --prefix "$REPOSITORY_ROOT/src/Raven.VSCode" run compile

echo
echo "Raven development environment build completed."
echo "  Configuration:  $CONFIGURATION"
echo "  Tool framework: $FRAMEWORK"
