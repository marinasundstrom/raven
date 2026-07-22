# Raven Distribution

Raven ships as two artifacts: a platform-specific Raven SDK archive and a
platform-independent VS Code extension. The SDK archive is the canonical
installation layout used by direct downloads and future package-manager
manifests.

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
      Raven.MSBuild.targets
```

The launchers require a compatible .NET SDK on `PATH`. Raven project builds
also use that SDK for MSBuild, reference assemblies, and targeting packs.

After extracting an archive, add its `bin` directory to `PATH`. The active SDK
can then be queried without parsing launcher paths:

```bash
rvn sdk path
```

Set `RAVEN_SDK_ROOT` to select an SDK explicitly. The directory must contain
both `VERSION` and `sdk/build/Raven.Language.targets`.

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

Validate a staged SDK before publishing it:

```bash
scripts/test-distribution.sh artifacts/distribution/raven-sdk-0.1.0-osx-arm64
```

Release automation should build these runtime identifiers:

- `win-x64`
- `win-arm64`
- `linux-x64`
- `linux-arm64`
- `osx-x64`
- `osx-arm64`

## VS Code extension

The extension contains a framework-dependent copy of the Raven language
server so editor features work without a platform-specific VSIX. Build it with:

```bash
scripts/package-vscode.sh
```

The extension resolves the compiler SDK from `raven.sdkPath` first and then by
running `rvn sdk path`. Build, run, and debug commands require the SDK, while
the bundled server can provide editor features independently.
