#!/usr/bin/env bash
# Verify that Raven's .NET 11 compiler host can build and run supported targets.

set -Euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BUILD_CONFIG="${BUILD_CONFIG:-Debug}"
SDK_VERSION="$(dotnet --version)"
SDK_MAJOR="${SDK_VERSION%%.*}"

if [[ ! "$SDK_MAJOR" =~ ^[0-9]+$ ]] || (( SDK_MAJOR < 11 )); then
  echo "Expected the active .NET SDK to be version 11 or newer; found $SDK_VERSION."
  exit 1
fi

echo "Active .NET SDK: $SDK_VERSION"
echo "Building the repository compiler host for net11.0"
dotnet build "$ROOT_DIR/src/Raven.Compiler/Raven.Compiler.csproj" \
  --framework net11.0 \
  --configuration "$BUILD_CONFIG" \
  /property:WarningLevel=0

for target_framework in net10.0 net11.0; do
  echo
  echo "Building Raven.Core and Raven.Macros for $target_framework"
  dotnet build "$ROOT_DIR/src/Raven.Core/Raven.Core.rvnproj" \
    --framework "$target_framework" \
    --configuration "$BUILD_CONFIG" \
    /property:WarningLevel=0
  dotnet build "$ROOT_DIR/src/Raven.Macros/Raven.Macros.rvnproj" \
    --framework "$target_framework" \
    --configuration "$BUILD_CONFIG" \
    /property:WarningLevel=0
done

echo
echo "Building representative projects with the net11.0 repository compiler host"
BUILD_CONFIG="$BUILD_CONFIG" "$ROOT_DIR/scripts/build-project-samples.sh" \
  data-literal-macros \
  timer-macro \
  runtime-async-net11

echo
echo "Running representative net10.0 projects"
dotnet "$ROOT_DIR/samples/projects/data-literal-macros/bin/$BUILD_CONFIG/net10.0/DataLiteralMacros.dll"
dotnet "$ROOT_DIR/samples/projects/timer-macro/bin/$BUILD_CONFIG/net10.0/TimerMacro.dll"

echo
echo "Running representative net11.0 project"
dotnet "$ROOT_DIR/samples/projects/runtime-async-net11/bin/$BUILD_CONFIG/net11.0/RuntimeAsyncNet11.dll"

echo
echo "Target-framework matrix passed with SDK $SDK_VERSION."
