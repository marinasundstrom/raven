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
  local package_entries

  if [[ ! -f "$package" ]]; then
    echo "Missing package: $package" >&2
    exit 1
  fi

  if [[ ! -f "$symbols" ]]; then
    echo "Missing symbol package: $symbols" >&2
    exit 1
  fi

  package_entries="$(unzip -Z1 "$package")"

  assert_archive_entry "$package" "README.md"
  assert_archive_entry "$package" "LICENSE"
  assert_archive_entry "$package" "lib/net10.0/$assembly_name.dll"
  assert_archive_entry "$package" "lib/net11.0/$assembly_name.dll"

  if [[ "$package_id" == "Raven.Core" || "$package_id" == "Raven.Macros" ]]; then
    assert_archive_entry "$package" "lib/net10.0/$assembly_name.xml"
    assert_archive_entry "$package" "lib/net11.0/$assembly_name.xml"
    assert_archive_entry "$package" "lib/net10.0/$assembly_name.docs/manifest.json"
    assert_archive_entry "$package" "lib/net11.0/$assembly_name.docs/manifest.json"
    for target_framework in net10.0 net11.0; do
      if ! grep -Eq "^lib/$target_framework/$assembly_name\\.docs/.+\\.md$" <<<"$package_entries"; then
        echo "Missing Markdown symbol documentation for $assembly_name $target_framework in $(basename "$package")" >&2
        exit 1
      fi
    done
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

sdk_package="$PACKAGE_DIR/Raven.Sdk.$VERSION.nupkg"
if [[ ! -f "$sdk_package" ]]; then
  echo "Missing package: $sdk_package" >&2
  exit 1
fi

for sdk_entry in \
  README.md \
  LICENSE \
  Sdk/Sdk.props \
  Sdk/Sdk.targets \
  build/Raven.Language.targets \
  tools/rvnc/rvnc.dll \
  tools/rvnc/rvnc.runtimeconfig.json; do
  assert_archive_entry "$sdk_package" "$sdk_entry"
done

if ! unzip -p "$sdk_package" tools/rvnc/rvnc.runtimeconfig.json | grep -Fq '"tfm": "net11.0"'; then
  echo "Raven.Sdk does not contain a .NET 11 compiler host." >&2
  exit 1
fi

sdk_nuspec="$(unzip -p "$sdk_package" Raven.Sdk.nuspec)"
if ! grep -Fq "<version>$VERSION</version>" <<<"$sdk_nuspec"; then
  echo "Incorrect version metadata in $(basename "$sdk_package")" >&2
  exit 1
fi
expected_repository_commit="$(git -C "$ROOT_DIR" rev-parse HEAD)"
if ! grep -Fq "commit=\"$expected_repository_commit\"" <<<"$sdk_nuspec"; then
  echo "Raven.Sdk was not packed from the current commit $expected_repository_commit." >&2
  exit 1
fi
if ! grep -Fq '<packageType name="MSBuildSdk"' <<<"$sdk_nuspec"; then
  echo "Raven.Sdk is not marked as an MSBuild project SDK." >&2
  exit 1
fi

template_package="$PACKAGE_DIR/Raven.Templates.$VERSION.nupkg"
if [[ ! -f "$template_package" ]]; then
  echo "Missing package: $template_package" >&2
  exit 1
fi

assert_archive_entry "$template_package" "README.md"
assert_archive_entry "$template_package" "LICENSE"
for template_name in console classlib web browser nano; do
  assert_archive_entry "$template_package" "content/$template_name/.template.config/template.json"
  assert_archive_entry "$template_package" "content/$template_name/RavenApp.rvnproj"
done
assert_archive_entry "$template_package" "content/browser/runtimeconfig.template.json"
assert_archive_entry "$template_package" "content/browser/wwwroot/index.html"
assert_archive_entry "$template_package" "content/browser/wwwroot/main.js"
assert_archive_entry "$template_package" "content/browser/wwwroot/styles.css"

template_nuspec="$(unzip -p "$template_package" Raven.Templates.nuspec)"
if ! grep -Fq "<version>$VERSION</version>" <<<"$template_nuspec"; then
  echo "Incorrect version metadata in $(basename "$template_package")" >&2
  exit 1
fi
if ! grep -Fq '<packageType name="Template"' <<<"$template_nuspec"; then
  echo "Raven.Templates is not marked as a NuGet template package." >&2
  exit 1
fi

TEMP_DIR="$(mktemp -d /tmp/raven-package-consumer.XXXXXX)"
web_server_pid=""
cleanup() {
  if [[ -n "$web_server_pid" ]]; then
    kill "$web_server_pid" 2>/dev/null || true
    wait "$web_server_pid" 2>/dev/null || true
  fi

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

template_cli_home="$TEMP_DIR/dotnet-cli-home"
template_packages="$TEMP_DIR/dotnet-packages"
mkdir -p "$template_cli_home" "$template_packages" "$TEMP_DIR/templates"

DOTNET_CLI_HOME="$template_cli_home" NUGET_PACKAGES="$template_packages" \
  dotnet new install "Raven.Templates@$VERSION" --add-source "$PACKAGE_DIR" --force >/dev/null

for template_name in console classlib web browser nano; do
  case "$template_name" in
    console) project_name="TemplateConsole" ;;
    classlib) project_name="TemplateClasslib" ;;
    web) project_name="TemplateWeb" ;;
    browser) project_name="TemplateBrowser" ;;
    nano) project_name="TemplateNano" ;;
  esac
  output_dir="$TEMP_DIR/templates/$template_name"
  if [[ "$template_name" == "web" ]]; then
    DOTNET_CLI_HOME="$template_cli_home" NUGET_PACKAGES="$template_packages" \
      dotnet new "raven-$template_name" --name "$project_name" --output "$output_dir" \
      --framework net11.0 >/dev/null
  else
    DOTNET_CLI_HOME="$template_cli_home" NUGET_PACKAGES="$template_packages" \
      dotnet new "raven-$template_name" --name "$project_name" --output "$output_dir" >/dev/null
  fi
  if [[ ! -f "$output_dir/$project_name.rvnproj" ]]; then
    echo "raven-$template_name did not create the expected project file." >&2
    exit 1
  fi
  if grep -R -Fq -e RavenApp -e RavenTargetFramework -e RavenSdkVersion \
      -e RavenHttpPort -e RavenHttpsPort "$output_dir"; then
    echo "raven-$template_name left an unsubstituted template token." >&2
    exit 1
  fi
  if ! grep -Fq "<Project Sdk=\"Raven.Sdk/$VERSION" "$output_dir/$project_name.rvnproj"; then
    echo "raven-$template_name did not select the matching Raven.Sdk version." >&2
    exit 1
  fi
done

if [[ ! -f "$TEMP_DIR/templates/classlib/src/Library.rvn" ]]; then
  echo "raven-classlib did not create src/Library.rvn." >&2
  exit 1
fi
for template_name in console classlib web; do
  case "$template_name" in
    console) project_name="TemplateConsole" ;;
    classlib) project_name="TemplateClasslib" ;;
    web) project_name="TemplateWeb" ;;
    browser) project_name="TemplateBrowser" ;;
  esac
  if ! grep -Fq '<TargetFramework>net11.0</TargetFramework>' \
    "$TEMP_DIR/templates/$template_name/$project_name.rvnproj"; then
    echo "raven-$template_name did not use the net11.0 default target framework." >&2
    exit 1
  fi
done
if ! grep -Fq '<TargetFramework>net10.0</TargetFramework>' \
  "$TEMP_DIR/templates/browser/TemplateBrowser.rvnproj"; then
  echo "raven-browser did not use the net10.0 default target framework." >&2
  exit 1
fi
if ! grep -Fq '<TargetFramework>netnano1.0</TargetFramework>' "$TEMP_DIR/templates/nano/TemplateNano.rvnproj"; then
  echo "raven-nano did not use the netnano1.0 default target framework." >&2
  exit 1
fi

cat > "$TEMP_DIR/NuGet.Config" <<EOF
<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <packageSources>
    <clear />
    <add key="raven-local" value="$PACKAGE_DIR" />
    <add key="nuget.org" value="https://api.nuget.org/v3/index.json" />
  </packageSources>
</configuration>
EOF

for template_name in console classlib web browser; do
  project_file="$(find "$TEMP_DIR/templates/$template_name" -maxdepth 1 -name '*.rvnproj' -print -quit)"
  template_build_log="$TEMP_DIR/template-$template_name-build.log"
  if ! DOTNET_CLI_HOME="$template_cli_home" NUGET_PACKAGES="$template_packages" \
    dotnet build "$project_file" --disable-build-servers \
    /property:WarningLevel=0 >"$template_build_log" 2>&1; then
    cat "$template_build_log" >&2
    exit 1
  fi
done

browser_framework_dir="$TEMP_DIR/templates/browser/bin/Debug/net10.0/wwwroot/_framework"
if [[ ! -f "$browser_framework_dir/dotnet.js" ]] ||
   ! find "$browser_framework_dir" -maxdepth 1 -name 'TemplateBrowser*.wasm' -print -quit | grep -q .; then
  echo "Packaged raven-browser template did not produce a browser WebAssembly app bundle." >&2
  exit 1
fi

web_log="$TEMP_DIR/template-web-run.log"
DOTNET_CLI_HOME="$template_cli_home" NUGET_PACKAGES="$template_packages" \
  dotnet run --project "$TEMP_DIR/templates/web/TemplateWeb.rvnproj" \
  --no-build --no-restore -- --urls http://127.0.0.1:0 >"$web_log" 2>&1 &
web_server_pid=$!
web_url=""
for _ in {1..60}; do
  web_url="$(sed -n 's/.*Now listening on: \(http:\/\/127\.0\.0\.1:[0-9][0-9]*\).*/\1/p' "$web_log" | head -1)"
  if [[ -n "$web_url" ]]; then
    break
  fi
  if ! kill -0 "$web_server_pid" 2>/dev/null; then
    cat "$web_log" >&2
    echo "Packaged raven-web template exited before listening." >&2
    exit 1
  fi
  sleep 0.5
done
if [[ -z "$web_url" ]]; then
  cat "$web_log" >&2
  echo "Packaged raven-web template did not start listening." >&2
  exit 1
fi
web_output="$(curl --fail --silent --show-error "$web_url/")"
if [[ "$web_output" != "Hello from Raven" ]]; then
  echo "Packaged raven-web template returned '$web_output'; expected 'Hello from Raven'." >&2
  exit 1
fi
kill "$web_server_pid"
wait "$web_server_pid" 2>/dev/null || true
web_server_pid=""

template_console_output="$(dotnet "$TEMP_DIR/templates/console/bin/Debug/net11.0/TemplateConsole.dll")"
if [[ "$template_console_output" != "Hello from Raven" ]]; then
  echo "Packaged raven-console template returned '$template_console_output'; expected 'Hello from Raven'." >&2
  exit 1
fi

template_publish_log="$TEMP_DIR/template-console-publish.log"
if ! DOTNET_CLI_HOME="$template_cli_home" NUGET_PACKAGES="$template_packages" \
  dotnet publish "$TEMP_DIR/templates/console/TemplateConsole.rvnproj" \
  -c Release \
  --no-restore \
  --disable-build-servers \
  /property:WarningLevel=0 >"$template_publish_log" 2>&1; then
  cat "$template_publish_log" >&2
  exit 1
fi
template_published_output="$(dotnet "$TEMP_DIR/templates/console/bin/Release/net11.0/publish/TemplateConsole.dll")"
if [[ "$template_published_output" != "Hello from Raven" ]]; then
  echo "Published raven-console template returned '$template_published_output'; expected 'Hello from Raven'." >&2
  exit 1
fi

classlib_raven_consumer="$TEMP_DIR/classlib-raven-consumer"
DOTNET_CLI_HOME="$template_cli_home" NUGET_PACKAGES="$template_packages" \
  dotnet new raven-console \
  --name ClasslibRavenConsumer \
  --output "$classlib_raven_consumer" >/dev/null
dotnet add "$classlib_raven_consumer/ClasslibRavenConsumer.rvnproj" reference \
  "$TEMP_DIR/templates/classlib/TemplateClasslib.rvnproj" >/dev/null
printf '%s\n' \
  'func Main() {' \
  '    System.Console.WriteLine(Greet())' \
  '}' \
  > "$classlib_raven_consumer/src/Main.rvn"

classlib_raven_run_log="$TEMP_DIR/classlib-raven-run.log"
if ! DOTNET_CLI_HOME="$template_cli_home" NUGET_PACKAGES="$template_packages" \
  dotnet run --project "$classlib_raven_consumer/ClasslibRavenConsumer.rvnproj" \
  --disable-build-servers \
  /property:WarningLevel=0 >"$classlib_raven_run_log" 2>&1; then
  cat "$classlib_raven_run_log" >&2
  exit 1
fi
if ! grep -Fxq "Hello from Raven" "$classlib_raven_run_log"; then
  cat "$classlib_raven_run_log" >&2
  echo "Raven class-library consumer did not print 'Hello from Raven'." >&2
  exit 1
fi

classlib_raven_publish_log="$TEMP_DIR/classlib-raven-publish.log"
if ! DOTNET_CLI_HOME="$template_cli_home" NUGET_PACKAGES="$template_packages" \
  dotnet publish "$classlib_raven_consumer/ClasslibRavenConsumer.rvnproj" \
  -c Release \
  --disable-build-servers \
  /property:WarningLevel=0 >"$classlib_raven_publish_log" 2>&1; then
  cat "$classlib_raven_publish_log" >&2
  exit 1
fi
classlib_raven_published_output="$(dotnet "$classlib_raven_consumer/bin/Release/net11.0/publish/ClasslibRavenConsumer.dll")"
if [[ "$classlib_raven_published_output" != "Hello from Raven" ]]; then
  echo "Published Raven class-library consumer returned '$classlib_raven_published_output'; expected 'Hello from Raven'." >&2
  exit 1
fi

classlib_csharp_consumer="$TEMP_DIR/classlib-csharp-consumer"
dotnet new console \
  --framework net11.0 \
  --no-restore \
  --output "$classlib_csharp_consumer" >/dev/null
dotnet add "$classlib_csharp_consumer/classlib-csharp-consumer.csproj" reference \
  "$TEMP_DIR/templates/classlib/TemplateClasslib.rvnproj" >/dev/null
printf '%s\n' \
  'Console.WriteLine(NamespaceMembers.Greet());' \
  > "$classlib_csharp_consumer/Program.cs"

classlib_csharp_run_log="$TEMP_DIR/classlib-csharp-run.log"
if ! DOTNET_CLI_HOME="$template_cli_home" NUGET_PACKAGES="$template_packages" \
  dotnet run --project "$classlib_csharp_consumer/classlib-csharp-consumer.csproj" \
  --disable-build-servers \
  /property:WarningLevel=0 >"$classlib_csharp_run_log" 2>&1; then
  cat "$classlib_csharp_run_log" >&2
  exit 1
fi
if ! grep -Fxq "Hello from Raven" "$classlib_csharp_run_log"; then
  cat "$classlib_csharp_run_log" >&2
  echo "C# class-library consumer did not print 'Hello from Raven'." >&2
  exit 1
fi

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

DOTNET_CLI_HOME="$template_cli_home" NUGET_PACKAGES="$template_packages" \
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

DOTNET_CLI_HOME="$template_cli_home" NUGET_PACKAGES="$template_packages" \
  dotnet build "$TEMP_DIR/consumer/consumer.csproj" \
  --no-restore \
  --disable-build-servers \
  /property:WarningLevel=0 >/dev/null

mkdir -p "$TEMP_DIR/analyzer-consumer/src"
for target_framework in net10.0 net11.0; do
  raven_consumer="$TEMP_DIR/raven-consumer-$target_framework"
  mkdir -p "$raven_consumer/src"
  printf '%s\n' \
    "<Project Sdk=\"Raven.Sdk/$VERSION\">" \
    '  <PropertyGroup>' \
    "    <TargetFramework>$target_framework</TargetFramework>" \
    '    <AssemblyName>PackageMacroConsumer</AssemblyName>' \
    '    <OutputType>Exe</OutputType>' \
    '  </PropertyGroup>' \
    '</Project>' \
    > "$raven_consumer/PackageMacroConsumer.rvnproj"

  printf '%s\n' \
    'import System.Console.*' \
    'import Raven.Macros.*' \
    '' \
    'func Main() {' \
    '    WriteLine(sha256Digest!("hello"))' \
    '}' \
    > "$raven_consumer/src/Main.rvn"

  raven_restore_log="$TEMP_DIR/raven-restore-$target_framework.log"
  if ! DOTNET_CLI_HOME="$template_cli_home" NUGET_PACKAGES="$template_packages" \
    dotnet restore "$raven_consumer/PackageMacroConsumer.rvnproj" \
    /property:WarningLevel=0 >"$raven_restore_log" 2>&1; then
    cat "$raven_restore_log" >&2
    exit 1
  fi

  raven_assets_file="$raven_consumer/obj/project.assets.json"
  for package_identity in "Raven.Core/$VERSION" "Raven.Macros/$VERSION" "Raven.CodeAnalysis/$VERSION"; do
    if ! grep -Fq "\"$package_identity\"" "$raven_assets_file"; then
      echo "Raven.Sdk did not resolve implicit package $package_identity for $target_framework" >&2
      exit 1
    fi
  done

  raven_build_log="$TEMP_DIR/raven-build-$target_framework.log"
  if ! DOTNET_CLI_HOME="$template_cli_home" NUGET_PACKAGES="$template_packages" \
    dotnet build "$raven_consumer/PackageMacroConsumer.rvnproj" \
    --no-restore \
    --disable-build-servers \
    /property:WarningLevel=0 >"$raven_build_log" 2>&1; then
    cat "$raven_build_log" >&2
    exit 1
  fi

  macro_output="$(dotnet "$raven_consumer/bin/Debug/$target_framework/PackageMacroConsumer.dll")"
  expected_digest="2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
  if [[ "$macro_output" != "$expected_digest" ]]; then
    echo "Packaged Raven.Macros $target_framework smoke test returned '$macro_output'; expected '$expected_digest'." >&2
    exit 1
  fi
done

printf '%s\n' \
  "<Project Sdk=\"Raven.Sdk/$VERSION\">" \
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
if ! DOTNET_CLI_HOME="$template_cli_home" NUGET_PACKAGES="$template_packages" \
  dotnet restore "$TEMP_DIR/analyzer-consumer/PackageAnalyzerConsumer.rvnproj" \
  /property:WarningLevel=0 >"$analyzer_restore_log" 2>&1; then
  cat "$analyzer_restore_log" >&2
  exit 1
fi

analyzer_build_log="$TEMP_DIR/analyzer-build.log"
if ! DOTNET_CLI_HOME="$template_cli_home" NUGET_PACKAGES="$template_packages" \
  dotnet build "$TEMP_DIR/analyzer-consumer/PackageAnalyzerConsumer.rvnproj" \
  --no-restore \
  --disable-build-servers \
  /property:WarningLevel=0 >"$analyzer_build_log" 2>&1; then
  cat "$analyzer_build_log" >&2
  exit 1
fi

if ! grep -Fq "RAV9036" "$analyzer_build_log"; then
  cat "$analyzer_build_log" >&2
  echo "Packaged Raven.Analyzers did not report the expected RAV9036 diagnostic." >&2
  exit 1
fi

mkdir -p "$TEMP_DIR/diagnostic-consumer/src"
printf '%s\n' \
  "<Project Sdk=\"Raven.Sdk/$VERSION\">" \
  '  <PropertyGroup>' \
  '    <TargetFramework>net10.0</TargetFramework>' \
  '    <OutputType>Exe</OutputType>' \
  '  </PropertyGroup>' \
  '</Project>' \
  > "$TEMP_DIR/diagnostic-consumer/DiagnosticConsumer.rvnproj"
printf '%s\n' 'System.Console.WriteLine("First")' \
  > "$TEMP_DIR/diagnostic-consumer/src/First.rvn"
printf '%s\n' 'System.Console.WriteLine("Second")' \
  > "$TEMP_DIR/diagnostic-consumer/src/Second.rvn"

diagnostic_build_log="$TEMP_DIR/diagnostic-build.log"
if DOTNET_CLI_HOME="$template_cli_home" NUGET_PACKAGES="$template_packages" \
  dotnet build "$TEMP_DIR/diagnostic-consumer/DiagnosticConsumer.rvnproj" \
  --disable-build-servers \
  /property:WarningLevel=0 >"$diagnostic_build_log" 2>&1; then
  cat "$diagnostic_build_log" >&2
  echo "The multiple top-level file diagnostic project unexpectedly built successfully." >&2
  exit 1
fi
if [[ "$(grep -c 'RAV1013' "$diagnostic_build_log")" -lt 2 ]]; then
  cat "$diagnostic_build_log" >&2
  echo "The SDK build did not report RAV1013 for both top-level source files." >&2
  exit 1
fi
if grep -Fq -e 'MSB3073' -e '--refs' "$diagnostic_build_log"; then
  cat "$diagnostic_build_log" >&2
  echo "The SDK build exposed the compiler command after a normal diagnostic failure." >&2
  exit 1
fi

echo "Validated Raven NuGet package family $VERSION in $PACKAGE_DIR"
