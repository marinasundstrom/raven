#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

# Build the C# seed from this checkout before compiling any Raven port sources.
# Use the repository targets and compiler, never an installed Raven SDK.
dotnet build src/Raven.Compiler/Raven.Compiler.csproj -f net11.0 -m:1 /property:WarningLevel=0 /property:UseRavenCoreReference=false
compiler_host="$ROOT_DIR/src/Raven.Compiler/bin/Debug/net11.0/rvnc.dll"
results_dir="${RAVEN_PARITY_RESULTS_DIR:-$ROOT_DIR/artifacts/compiler-parity}"
mkdir -p "$results_dir"

{
  git rev-parse HEAD
  git status --short
  dotnet --version
  shasum -a 256 "$compiler_host" src/Raven.Compiler/bin/Debug/net11.0/Raven.CodeAnalysis.dll
} > "$results_dir/provenance.txt"

for implementation in CSharp Raven; do
  dotnet test "test/CompilerParity/$implementation/Raven.CompilerParity.$implementation.Tests.csproj" \
    -m:1 /property:WarningLevel=0 \
    "/property:RavenCompilerHost=$compiler_host" \
    --logger 'trx;LogFileName=results.trx' \
    --results-directory "$results_dir/$implementation"
done

shasum -a 256 \
  src/Raven.CodeAnalysis/bin/Debug/net11.0/Raven.CodeAnalysis.dll \
  src/Raven.CodeAnalysis.Raven/bin/Debug/net11.0/Raven.CodeAnalysis.Raven.dll \
  >> "$results_dir/provenance.txt"
