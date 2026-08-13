# Raven Distribution

Raven ships platform-specific SDK archives, a platform-independent VS Code
extension, and a lockstep family of compiler libraries as NuGet packages. The
SDK archive is the canonical installation layout used by direct downloads and
future package-manager manifests.

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
curl -fsSL https://raw.githubusercontent.com/marinasundstrom/raven/main/scripts/install-raven.sh | sh -s -- 0.1.0
```

```powershell
./install-raven.ps1 -Version 0.1.0
```

Both installers verify the archive against the release's `SHA256SUMS` file and
install versioned SDK files under `~/.raven` by default.
Set `RAVEN_INSTALL_ROOT` to choose another installation directory.

## Building an SDK archive

Run the package script with a .NET runtime identifier and version:

```bash
scripts/package-sdk.sh osx-arm64 0.1.0
scripts/package-sdk.sh linux-x64 0.1.0
scripts/package-sdk.sh win-x64 0.1.0
```

Artifacts are written to `artifacts/distribution` by default. Override the
target framework with `RAVEN_PACKAGE_TFM` and the output directory with
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

The dispatch form has separate opt-in switches for publishing a GitHub release
and publishing the NuGet package family. Either publishing operation requires
the workflow to be dispatched against the matching `v<version>` tag. This keeps
building, creating a GitHub release, and pushing to NuGet.org explicit release
operator decisions while reusing one validated artifact set.

## NuGet packages

Raven's initial NuGet family contains:

- `Raven.Core`: core types and runtime support for compiled Raven programs.
- `Raven.Macros`: Raven's standard compiler macros.
- `Raven.CodeAnalysis`: public syntax, semantic, workspace, diagnostic, and
  emission APIs.
- `Raven.Analyzers`: recommended naming and style analyzers and code fixes,
  delivered through NuGet's `analyzers/dotnet` asset convention.

All packages in a release are built from the same commit and receive the same
version. `Raven.Macros` carries a package dependency on the matching
`Raven.CodeAnalysis` version. `Raven.Analyzers` intentionally carries no
runtime dependency: it binds to the compiler host's matching
`Raven.CodeAnalysis` assembly when the analyzer asset is loaded.

Build and validate the packages locally with:

```bash
scripts/package-nuget.sh 0.1.0-preview.1
```

Packages and symbol packages are written to `artifacts/packages`. Set
`RAVEN_NUGET_OUTPUT` to use another directory. Validation checks package
contents and metadata, then restores and builds isolated consumer projects
from the local package directory. The Raven consumer executes a packaged macro
and asserts that a packaged analyzer reports its expected diagnostic.

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

The extension resolves the compiler SDK from `raven.sdkPath` first and then by
running `rvn sdk path`. Build, run, and debug commands require the SDK, while
the bundled server can provide editor features independently.
