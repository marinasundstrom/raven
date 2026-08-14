# Raven Distribution

Raven ships platform-specific SDK archives, a platform-independent VS Code
extension, and a lockstep family of compiler libraries as NuGet packages. The
SDK archive is the canonical installation layout used by direct downloads and
future package-manager manifests.

The MVP distribution is a .NET 11-hosted toolchain and requires a compatible
.NET 11 SDK. All processes and plugins loaded by the distributed compiler use
that host line. This does not restrict application targets: target-specific
Raven.Core and Raven.Macros package assets allow the .NET 11 toolchain to build
both net10.0 and net11.0 projects when their targeting packs are installed.

## SDK layout

An installed SDK has the following stable structure:

```text
raven-sdk-<version>-<rid>/
  VERSION
  bin/
    rvn
    rvnc
    raven-language-server
  tools/
    rvn/
    rvnc/
    language-server/
  sdk/
    Raven.Core.dll
    build/
      Raven.Language.targets
      Raven.MSBuild.props
      Raven.nanoFramework.props
      Raven.nanoFramework.targets
```

The launchers require a compatible .NET SDK on `PATH`. Raven project builds
also use that SDK for MSBuild, reference assemblies, and targeting packs.

After extracting an archive, add its `bin` directory to `PATH`. The active SDK
can then be queried without parsing launcher paths:

```bash
rvn sdk path
rvn doctor
```

`rvn doctor` verifies that a compatible .NET SDK is available and that the
active Raven SDK contains the compiler, language server, core library, and
MSBuild targets.

Set `RAVEN_SDK_ROOT` to select an SDK explicitly. The directory must contain
both `VERSION` and `sdk/build/Raven.Language.targets`.

Release builds can be installed directly with the platform installer:

```bash
curl -fsSL https://github.com/marinasundstrom/raven/releases/download/v0.1.0-preview.9/install-raven.sh \
  | sh -s -- 0.1.0-preview.9
```

```powershell
$version = "0.1.0-preview.9"
Invoke-WebRequest "https://github.com/marinasundstrom/raven/releases/download/v$version/install-raven.ps1" -OutFile install-raven.ps1
./install-raven.ps1 -Version $version
```

Both installers verify the archive against the release's `SHA256SUMS` file and
install versioned SDK files under `~/.raven` by default.
Set `RAVEN_INSTALL_ROOT` to choose another installation directory.
Add `~/.raven/bin` to PATH after installation, then run `rvn doctor`.

## Building an SDK archive

Run the package script with a .NET runtime identifier and version:

```bash
scripts/package-sdk.sh osx-arm64 0.1.0
scripts/package-sdk.sh linux-x64 0.1.0
scripts/package-sdk.sh win-x64 0.1.0
```

Artifacts are written to `artifacts/distribution` by default. The distributable
toolchain is fixed to net11.0 for this MVP. Override the output directory with
`RAVEN_PACKAGE_OUTPUT`.

The packaging scripts regenerate compiler sources before building, so they are
safe to run from a clean checkout without relying on ignored build outputs.

Validate a staged SDK before publishing it:

```bash
scripts/test-distribution.sh artifacts/distribution/raven-sdk-0.1.0-osx-arm64
scripts/test-distribution.sh --structure-only artifacts/distribution/raven-sdk-0.1.0-win-arm64
```

Use `--structure-only` when inspecting an archive that cannot execute on the
current operating system or architecture. Release automation structurally
validates every archive and executes an additional smoke test for its native
Linux x64 artifact.

Release automation should build these runtime identifiers:

- `win-x64`
- `win-arm64`
- `linux-x64`
- `linux-arm64`
- `osx-x64`
- `osx-arm64`

The `Distribution` GitHub Actions workflow builds all six archives and the
VSIX together with the NuGet package family. Distribution is deliberately a
manual process: neither a branch push nor a tag push starts the workflow. Start
`Distribution` from the GitHub Actions UI, select the commit or tag to build,
and provide the version explicitly. By default the run only produces retained
workflow artifacts and does not publish anything externally.

## Release procedure

A release is one immutable Git commit. That commit must contain the code,
generated files, package version, changelog entry, installation examples, and
all other versioned documentation. Prepare those references from a clean
worktree:

```bash
scripts/prepare-release.sh
```

Until the project deliberately transitions to its first stable major release,
Raven uses one monotonically increasing prerelease counter:
`0.1.0-preview.N`. Release preparation derives the next counter from
`global.json`; for example, both `preview.8` and the historical `preview.8.1`
advance to `preview.9`. Do not add another nested patch counter. An optional
argument such as `scripts/prepare-release.sh 0.1.0-preview.9` asserts the
expected derived version but cannot override it. Moving to `1.0.0` or another
release line is an explicit project decision and requires updating this policy.

The script replaces the previously selected release version in tracked files,
creates the dated changelog section, and validates the known release references.
Review its complete diff and finish the changelog before committing. Do not tag
an earlier code commit and then add release references in a later commit.

Review the diff, finish the changelog, and commit every intended release change.
Then run the release checks against that clean commit:

```bash
scripts/validate-release.sh VERSION --require-clean
scripts/test-release.sh
scripts/package-nuget.sh VERSION
```

`test-release.sh` runs the normal baseline, the isolated runtime/emission suite,
and all language-server unit, integration, and performance-test projects. If a
check requires a fix, commit the fix and repeat the checks so the tested commit
remains the release candidate. Push it and make sure its ordinary CI checks
pass. Only then tag that exact commit and push the tag:

```bash
git tag vVERSION
git push origin vVERSION
```

Dispatch `Distribution` against `vVERSION`, never against the branch name, when
either publication switch is enabled. The workflow verifies that the tag points
to the checked-out commit, checks that the NuGet version is still unused, and
runs the complete release test gate on that exact commit before any archive,
VSIX, or NuGet package is built. Package validation also checks that
`Raven.Sdk` records the same Git commit. NuGet publication deliberately fails
on duplicate versions; published versions are immutable and must never be
silently skipped.

The repository's `NuGet.Config` puts `artifacts/packages` before NuGet.org for
source-build testing. NuGet's global package cache is keyed only by package ID
and version, so rebuilding a local package with the same version does not update
an already restored cache entry. Use a fresh `NUGET_PACKAGES` directory when
validating repacked local artifacts or the public release, and inspect
`.nupkg.metadata` when package provenance is uncertain. A local restore is not
evidence of what NuGet.org contains.

The dispatch form has separate opt-in switches for publishing a GitHub release
and publishing the NuGet package family. Either publishing operation requires
the workflow to be dispatched against the matching `v<version>` tag. This keeps
building, creating a GitHub release, and pushing to NuGet.org explicit release
operator decisions while reusing one validated artifact set. Versions with a
SemVer prerelease suffix, such as `0.1.0-preview.3`, create a GitHub prerelease.

After publication, manually run the `Installation verification` workflow with
the published version. It downloads the public release rather than reusing
workflow artifacts, installs and exercises the SDK on Windows, Linux, and
macOS across the published architectures, and installs the checksum-verified
VSIX into clean portable VS Code instances on all three operating systems.

## NuGet packages

Raven's initial NuGet family contains:

- `Raven.Core`: core types and runtime support for compiled Raven programs.
- `Raven.Macros`: Raven's standard compiler macros.
- `Raven.CodeAnalysis`: public syntax, semantic, workspace, diagnostic, and
  emission APIs.
- `Raven.Analyzers`: recommended naming and style analyzers and code fixes,
  delivered through NuGet's `analyzers/dotnet` asset convention.
- `Raven.Sdk`: the NuGet-resolved MSBuild Project SDK containing the Raven
  compiler host and language targets. It builds on `Microsoft.NET.Sdk` and
  adds exact-version implicit references to `Raven.Core` and `Raven.Macros`.
- `Raven.Templates`: project templates for the standard `dotnet new` CLI,
  with console, class-library, ASP.NET Core, and .NET nanoFramework variants.

All packages in a release are built from the same commit and receive the same
version. `Raven.Macros` carries a package dependency on the matching
`Raven.CodeAnalysis` version. `Raven.Analyzers` intentionally carries no
runtime dependency: it binds to the compiler host's matching
`Raven.CodeAnalysis` assembly when the analyzer asset is loaded.
`Raven.Sdk` is the .NET CLI entry point for `.rvnproj` projects. Standalone
projects generated by `Raven.Templates` and `rvn init` pin
`<Project Sdk="Raven.Sdk/VERSION">`. Repositories can instead use the shorter
`<Project Sdk="Raven.Sdk">` everywhere and select `Raven.Sdk` once under the
`msbuild-sdks` section of `global.json`. Neither form requires users to
configure `LanguageTargets`, compiler paths, or SDK installation roots. The
SDK, templates, compiler, Core, and Macros versions move together.

Build and validate the packages locally with:

```bash
scripts/package-nuget.sh 0.1.0-preview.1
```

Packages and symbol packages are written to `artifacts/packages`. Set
`RAVEN_NUGET_OUTPUT` to use another directory. Validation checks package
contents and metadata, then restores and builds isolated consumer projects
from the local package directory. The Raven consumer resolves Core and Macros
implicitly through `Raven.Sdk`, executes a packaged macro, and asserts that a
packaged analyzer reports its expected diagnostic.
It also installs `Raven.Templates` into an isolated .NET CLI home and
materializes all four template variants without changing the operator's
machine-wide template registrations. Console, class-library, and Web projects
are built; the console result is also executed. The nanoFramework template is
validated structurally but is not compiled as part of package validation,
because its metadata processor requires a separate Mono toolchain on Unix.
The class-library check creates a second Raven application with a normal
`ProjectReference`, runs and publishes that application, and verifies the
library's public API from a C# project as well.

After publication, install and use the templates with:

```bash
dotnet new install Raven.Templates@VERSION
dotnet new raven-console -n HelloRaven
cd HelloRaven
dotnet run
dotnet new raven-classlib -n MyLibrary
dotnet new raven-web -n RavenWeb
dotnet new raven-nano -n RavenBlinky
```

To use the generated class library from another project, add an ordinary .NET
project reference:

```bash
dotnet add HelloRaven/HelloRaven.rvnproj reference MyLibrary/MyLibrary.rvnproj
```

Public top-level Raven functions are imported as namespace members by Raven
consumers. Other .NET languages can call their generated static container;
for the global namespace used by the template, `Greet()` is emitted as
`NamespaceMembers.Greet()`.

### Package a Raven class library

A Raven class library uses the standard .NET packing workflow. Add the normal
NuGet metadata to the project and run `dotnet pack`:

```xml
<Project Sdk="Raven.Sdk/VERSION">
  <PropertyGroup>
    <TargetFramework>net11.0</TargetFramework>
    <OutputType>Library</OutputType>
    <PackageId>Contoso.RavenUtilities</PackageId>
    <Version>1.0.0</Version>
    <Authors>Contoso</Authors>
    <Description>Reusable Raven utilities.</Description>
  </PropertyGroup>
</Project>
```

```bash
dotnet pack --configuration Release --output artifacts/packages
```

Raven libraries generate both documentation formats by default. `dotnet pack`
automatically includes the assembly, .NET-compatible XML documentation, and
the complete Raven Markdown sidecar tree:

```text
lib/net11.0/Contoso.RavenUtilities.dll
lib/net11.0/Contoso.RavenUtilities.xml
lib/net11.0/Contoso.RavenUtilities.docs/
  manifest.json
  invariant/
    symbols/
      ...
```

Keep the `.xml` file and `.docs` directory adjacent to the assembly. Raven
tooling prefers a matching Markdown symbol file and falls back to XML. C# and
other conventional .NET tooling can consume the XML file. Consumers do not
need to configure documentation paths: restore and build copy available
sidecars from copy-local package references beside the referenced DLL.

The SDK handles its own generated sidecar automatically. To package a manually
maintained Markdown tree from another location, append a target to the standard
NuGet pack hook. Preserve the directory name, manifest, and all relative paths:

```xml
<PropertyGroup>
  <GenerateMarkdownDocumentationFile>false</GenerateMarkdownDocumentationFile>
  <TargetsForTfmSpecificContentInPackage>
    $(TargetsForTfmSpecificContentInPackage);IncludeCustomRavenDocumentation
  </TargetsForTfmSpecificContentInPackage>
</PropertyGroup>

<Target Name="IncludeCustomRavenDocumentation">
  <ItemGroup>
    <TfmSpecificPackageFile Include="docs/$(AssemblyName).docs/**/*">
      <PackagePath>
        lib/$(TargetFramework)/$(AssemblyName).docs/%(RecursiveDir)%(Filename)%(Extension)
      </PackagePath>
    </TfmSpecificPackageFile>
  </ItemGroup>
</Target>
```

Do not flatten the Markdown directory or omit `manifest.json`; symbol lookup
depends on that structure. See the
[External Documentation Sidecars](https://github.com/marinasundstrom/raven/blob/main/docs/compiler/design/external-documentation-sidecars.md)
design note for the format contract and the [project system](project-system.md)
for documentation-related properties.

Replace `VERSION` with the release version being installed. Specifying it is
required while Raven is distributed only as prerelease packages.

The MVP intentionally uses the base `Raven.Sdk` plus a framework reference for
minimal ASP.NET Core applications. A dedicated `Raven.Sdk.Web` remains future
work for Razor, static web assets, and the complete Web SDK publishing model.
The nanoFramework template remains on its specialized target and does not take
the desktop `Raven.Core` or `Raven.Macros` packages, which currently target
`net10.0` and `net11.0`.

### MVP boundary and follow-up

The first installable SDK release is complete when the manually dispatched
release publishes one lockstep version of the NuGet family, standalone SDK
archives/installers, and VSIX; the separate installation workflow must then
create, restore, build, run, and publish generated projects through the public
distribution channels on the supported operating systems.

The following improvements are intentionally outside that MVP:

- a dedicated `Raven.Sdk.Web` for Razor, static web assets, and all Web SDK
  publish defaults;
- a nanoFramework-compatible Raven.Core and standard-macro surface, potentially
  exposed through `Raven.Sdk.nanoFramework`;
- a .NET global tool and package-manager manifests for installing `rvn`;
- VS Code Marketplace publication (the VSIX remains a GitHub release asset);
- a .NET workload manifest, which is unnecessary while the compiler toolchain
  can be restored as a normal NuGet Project SDK; and
- side-by-side installation and selection of Raven SDK versions. The intended
  default is the latest compatible installed Raven SDK unless a project pins a
  version; the resolver policy is a post-MVP concern; and
- signing/notarization and additional supply-chain provenance beyond release
  checksums and NuGet Trusted Publishing.

NuGet.org publication only runs when an operator manually dispatches the
workflow against the matching `v<version>` tag and enables `publish_nuget`.
The publishing job uses NuGet Trusted Publishing for package owner `marna.li`,
repository `marinasundstrom/raven`, and workflow file `distribution.yml`; it
does not use a stored, long-lived API key. Leave the trusted publisher's
environment field empty because this workflow does not declare a GitHub
environment.

## VS Code extension

The extension contains a framework-dependent copy of the Raven language
server so editor features work without a platform-specific VSIX. Build it with:

```bash
scripts/package-vscode.sh 0.1.0
```

Install the published preview directly from GitHub Releases:

```bash
curl -fLO https://github.com/marinasundstrom/raven/releases/download/v0.1.0-preview.9/raven-vscode.vsix
code --install-extension raven-vscode.vsix --force
```

The extension resolves the compiler SDK from `raven.sdkPath` first and then by
running `rvn sdk path`. Build, run, and debug commands require the SDK, while
the bundled server can provide editor features independently.
For GUI sessions that do not inherit the shell PATH, set `raven.sdkPath` to the
versioned directory reported by `rvn sdk path`.

The TypeScript extension host and its runtime dependencies are bundled into a
single production JavaScript entry point. The VSIX excludes `node_modules`,
source files, source maps, and language-server symbols while retaining the
framework-dependent server binaries, grammar, configuration, README, and MIT
license.
