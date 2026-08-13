#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
VERSION="${1:-}"
PACKAGE_DIR="${2:-$ROOT_DIR/artifacts/packages}"

if [[ -z "$VERSION" ]]; then
  echo "Usage: scripts/test-nuget-packages.sh <version> [package-directory]" >&2
  exit 1
fi

if [[ ! -d "$PACKAGE_DIR" ]]; then
  echo "Package directory does not exist: $PACKAGE_DIR" >&2
  exit 1
fi

PACKAGE_DIR="$(cd "$PACKAGE_DIR" && pwd)"

assert_archive_entry() {
  local archive="$1"
  local entry="$2"
  local archive_entries
  archive_entries="$(unzip -Z1 "$archive")"
  if ! grep -Fxq "$entry" <<<"$archive_entries"; then
    echo "Missing '$entry' in $(basename "$archive")" >&2
    exit 1
  fi
}

assert_package() {
  local package_id="$1"
  local assembly_name="$2"
  local package="$PACKAGE_DIR/$package_id.$VERSION.nupkg"
  local symbols="$PACKAGE_DIR/$package_id.$VERSION.snupkg"

  if [[ ! -f "$package" ]]; then
    echo "Missing package: $package" >&2
    exit 1
  fi

  if [[ ! -f "$symbols" ]]; then
    echo "Missing symbol package: $symbols" >&2
    exit 1
  fi

  assert_archive_entry "$package" "README.md"
  assert_archive_entry "$package" "LICENSE"
  assert_archive_entry "$package" "lib/net10.0/$assembly_name.dll"
  assert_archive_entry "$package" "lib/net11.0/$assembly_name.dll"

  if [[ "$package_id" == "Raven.Core" || "$package_id" == "Raven.Macros" ]]; then
    assert_archive_entry "$package" "lib/net10.0/$assembly_name.xml"
    assert_archive_entry "$package" "lib/net11.0/$assembly_name.xml"
    assert_archive_entry "$package" "lib/net10.0/$assembly_name.docs/manifest.json"
    assert_archive_entry "$package" "lib/net11.0/$assembly_name.docs/manifest.json"
  fi

  local nuspec
  nuspec="$(unzip -p "$package" "$package_id.nuspec")"
  if ! grep -Fq "<version>$VERSION</version>" <<<"$nuspec"; then
    echo "Incorrect version metadata in $(basename "$package")" >&2
    exit 1
  fi
  if grep -Fq "<description>Package Description</description>" <<<"$nuspec"; then
    echo "Placeholder description remains in $(basename "$package")" >&2
    exit 1
  fi
}

assert_package "Raven.CodeAnalysis" "Raven.CodeAnalysis"
assert_package "Raven.Core" "Raven.Core"
assert_package "Raven.Macros" "Raven.Macros"

analyzer_package="$PACKAGE_DIR/Raven.Analyzers.$VERSION.nupkg"
if [[ ! -f "$analyzer_package" ]]; then
  echo "Missing package: $analyzer_package" >&2
  exit 1
fi

assert_archive_entry "$analyzer_package" "README.md"
assert_archive_entry "$analyzer_package" "LICENSE"
assert_archive_entry "$analyzer_package" "analyzers/dotnet/Raven.Analyzers.dll"
assert_archive_entry "$analyzer_package" "analyzers/dotnet/Raven.Analyzers.pdb"
if ! unzip -p "$analyzer_package" README.md | grep -Fq "Recommended convention and style analyzers"; then
  echo "Raven.Analyzers package does not contain its package-specific README." >&2
  exit 1
fi

analyzer_nuspec="$(unzip -p "$analyzer_package" Raven.Analyzers.nuspec)"
if ! grep -Fq "<version>$VERSION</version>" <<<"$analyzer_nuspec"; then
  echo "Incorrect version metadata in $(basename "$analyzer_package")" >&2
  exit 1
fi
if grep -Fq "<dependency id=\"Raven.CodeAnalysis\"" <<<"$analyzer_nuspec"; then
  echo "Raven.Analyzers must use the compiler host's Raven.CodeAnalysis assembly." >&2
  exit 1
fi

macros_nuspec="$(unzip -p "$PACKAGE_DIR/Raven.Macros.$VERSION.nupkg" Raven.Macros.nuspec)"
if ! grep -Fq "<dependency id=\"Raven.CodeAnalysis\" version=\"$VERSION\"" <<<"$macros_nuspec"; then
  echo "Raven.Macros must depend on the matching Raven.CodeAnalysis package version." >&2
  exit 1
fi

TEMP_DIR="$(mktemp -d /tmp/raven-package-consumer.XXXXXX)"
cleanup() {
  if [[ "${RAVEN_KEEP_PACKAGE_TEST_TEMP:-false}" == "true" ]]; then
    echo "Preserved package-test workspace: $TEMP_DIR" >&2
    return
  fi

  case "$TEMP_DIR" in
    /tmp/raven-package-consumer.*) rm -rf "$TEMP_DIR" ;;
    *) echo "Refusing to remove unexpected temporary path: $TEMP_DIR" >&2 ;;
  esac
}
trap cleanup EXIT

dotnet new console \
  --framework net10.0 \
  --no-restore \
  --output "$TEMP_DIR/consumer" >/dev/null

dotnet add "$TEMP_DIR/consumer/consumer.csproj" package Raven.Core \
  --version "[$VERSION]" \
  --source "$PACKAGE_DIR" \
  --no-restore >/dev/null

dotnet add "$TEMP_DIR/consumer/consumer.csproj" package Raven.Macros \
  --version "[$VERSION]" \
  --source "$PACKAGE_DIR" \
  --no-restore >/dev/null

dotnet restore "$TEMP_DIR/consumer/consumer.csproj" \
  --source "$PACKAGE_DIR" \
  --source https://api.nuget.org/v3/index.json \
  /property:WarningLevel=0 >/dev/null

assets_file="$TEMP_DIR/consumer/obj/project.assets.json"
for package_identity in "Raven.Core/$VERSION" "Raven.Macros/$VERSION" "Raven.CodeAnalysis/$VERSION"; do
  if ! grep -Fq "\"$package_identity\"" "$assets_file"; then
    echo "Consumer restore did not resolve $package_identity" >&2
    exit 1
  fi
done

dotnet build "$TEMP_DIR/consumer/consumer.csproj" \
  --no-restore \
  /property:WarningLevel=0 >/dev/null

mkdir -p "$TEMP_DIR/raven-consumer/src"
mkdir -p "$TEMP_DIR/analyzer-consumer/src"
mkdir -p "$TEMP_DIR/compiler-host"
cp -R "$ROOT_DIR/src/Raven.Compiler/bin/Release/net10.0/." "$TEMP_DIR/compiler-host/"
printf '%s\n' \
  '<Project Sdk="Microsoft.NET.Sdk">' \
  '  <PropertyGroup>' \
  '    <TargetFramework>net10.0</TargetFramework>' \
  '    <AssemblyName>PackageMacroConsumer</AssemblyName>' \
  '    <OutputType>Exe</OutputType>' \
  '  </PropertyGroup>' \
  '  <ItemGroup>' \
  "    <PackageReference Include=\"Raven.Macros\" Version=\"[$VERSION]\" />" \
  '  </ItemGroup>' \
  '</Project>' \
  > "$TEMP_DIR/raven-consumer/PackageMacroConsumer.rvnproj"

printf '%s\n' \
  'import System.Console.*' \
  'import Raven.Macros.*' \
  '' \
  'func Main() {' \
  '    WriteLine(sha256Digest!("hello"))' \
  '}' \
  > "$TEMP_DIR/raven-consumer/src/Main.rvn"

raven_restore_log="$TEMP_DIR/raven-restore.log"
if ! dotnet restore "$TEMP_DIR/raven-consumer/PackageMacroConsumer.rvnproj" \
  --source "$PACKAGE_DIR" \
  --source https://api.nuget.org/v3/index.json \
  /property:LanguageTargets="$ROOT_DIR/build/Raven.Language.targets" \
  /property:WarningLevel=0 >"$raven_restore_log" 2>&1; then
  cat "$raven_restore_log" >&2
  exit 1
fi

raven_build_log="$TEMP_DIR/raven-build.log"
if ! dotnet build "$TEMP_DIR/raven-consumer/PackageMacroConsumer.rvnproj" \
  --no-restore \
  /property:LanguageTargets="$ROOT_DIR/build/Raven.Language.targets" \
  /property:RavenCompilerHost="$TEMP_DIR/compiler-host/rvnc.dll" \
  /property:RavenCoreReferencePath="$ROOT_DIR/src/Raven.Core/bin/Release/net10.0/Raven.Core.dll" \
  /property:WarningLevel=0 >"$raven_build_log" 2>&1; then
  cat "$raven_build_log" >&2
  exit 1
fi

macro_output="$(dotnet "$TEMP_DIR/raven-consumer/bin/Debug/net10.0/PackageMacroConsumer.dll")"
expected_digest="2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
if [[ "$macro_output" != "$expected_digest" ]]; then
  echo "Packaged Raven.Macros smoke test returned '$macro_output'; expected '$expected_digest'." >&2
  exit 1
fi

printf '%s\n' \
  '<Project Sdk="Microsoft.NET.Sdk">' \
  '  <PropertyGroup>' \
  '    <TargetFramework>net10.0</TargetFramework>' \
  '    <AssemblyName>PackageAnalyzerConsumer</AssemblyName>' \
  '    <OutputType>Exe</OutputType>' \
  '  </PropertyGroup>' \
  '  <ItemGroup>' \
  "    <PackageReference Include=\"Raven.Analyzers\" Version=\"[$VERSION]\" />" \
  '  </ItemGroup>' \
  '</Project>' \
  > "$TEMP_DIR/analyzer-consumer/PackageAnalyzerConsumer.rvnproj"

printf '%s\n' \
  'func Main() {' \
  '    while true { }' \
  '}' \
  > "$TEMP_DIR/analyzer-consumer/src/Main.rvn"

printf '%s\n' \
  'root = true' \
  '' \
  '[*.rvn]' \
  'dotnet_diagnostic.RAV9036.severity = warning' \
  > "$TEMP_DIR/analyzer-consumer/.editorconfig"

analyzer_restore_log="$TEMP_DIR/analyzer-restore.log"
if ! dotnet restore "$TEMP_DIR/analyzer-consumer/PackageAnalyzerConsumer.rvnproj" \
  --source "$PACKAGE_DIR" \
  --source https://api.nuget.org/v3/index.json \
  /property:LanguageTargets="$ROOT_DIR/build/Raven.Language.targets" \
  /property:WarningLevel=0 >"$analyzer_restore_log" 2>&1; then
  cat "$analyzer_restore_log" >&2
  exit 1
fi

analyzer_build_log="$TEMP_DIR/analyzer-build.log"
if ! dotnet build "$TEMP_DIR/analyzer-consumer/PackageAnalyzerConsumer.rvnproj" \
  --no-restore \
  /property:LanguageTargets="$ROOT_DIR/build/Raven.Language.targets" \
  /property:RavenCompilerHost="$TEMP_DIR/compiler-host/rvnc.dll" \
  /property:RavenCoreReferencePath="$ROOT_DIR/src/Raven.Core/bin/Release/net10.0/Raven.Core.dll" \
  /property:WarningLevel=0 >"$analyzer_build_log" 2>&1; then
  cat "$analyzer_build_log" >&2
  exit 1
fi

if ! grep -Fq "RAV9036" "$analyzer_build_log"; then
  cat "$analyzer_build_log" >&2
  echo "Packaged Raven.Analyzers did not report the expected RAV9036 diagnostic." >&2
  exit 1
fi

echo "Validated Raven NuGet package family $VERSION in $PACKAGE_DIR"
