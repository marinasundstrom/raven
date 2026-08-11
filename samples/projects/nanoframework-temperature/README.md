# nanoFramework temperature monitor

This is Raven's compile-and-package MVP for .NET nanoFramework. It reads a DHT
temperature sensor, models the result as a closed `TemperatureState` union, and
uses exhaustive patterns to drive a GPIO alarm pin. The pure classification
decision remains separate from sensor and GPIO effects.

The sample currently proves this path:

```text
Program.rvn -> Raven managed assembly -> nanoFramework NFMRK2 image
```

It does not yet deploy the image, run it in nanoCLR, or prove board-specific pin
configuration.

## Prerequisites

- a .NET SDK capable of building Raven's `net10.0` compiler
- the `nuget` command-line client
- Mono, used to run the current metadata processor CLI

The package versions in `packages.config` record the integration snapshot used
by this MVP. They are deliberately explicit while nanoFramework support remains
an investigation target.

## Build and package

From this directory:

```bash
./build.sh
```

The script restores its packages into `.packages/`, builds `rvnc` if needed,
compiles with an explicit nanoFramework reference closure, and writes:

- `artifacts/NanoFrameworkTemperature.dll` &ndash; Raven's standard managed output
- `artifacts/NanoFrameworkTemperature.pe` &ndash; the nanoFramework `NFMRK2` image

`NANOFRAMEWORK_PACKAGES_DIR`, `OUTPUT_DIR`, `RAVEN_COMPILER_DLL`,
`NUGET_COMMAND`, and `MONO_COMMAND` can override the default tool and output
locations.

The next MVP step is to deploy the `.pe` image with the matching firmware and
dependency assemblies, then validate the state transitions and GPIO output in
nanoCLR or on representative hardware.
