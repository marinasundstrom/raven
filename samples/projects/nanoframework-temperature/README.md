# .NET nanoFramework temperature monitor

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
- Mono, used to run the current metadata processor CLI

`NanoFrameworkTemperature.rvnproj` is a standard SDK-style Raven project. It
selects `netnano1.0` and references the DHT device package normally; NuGet
restores the GPIO, UnitsNet, and other managed dependencies transitively while
Raven's target profile supplies the core library and metadata processor.
The sample uses the nanoFramework 2.0 preview DHT package line because those
packages publish proper `netnano1.0` assets; the older stable package line is
exposed to PackageReference as an unversioned .NET Framework asset.

## Build and package

From this directory:

```bash
./build.sh
```

The script restores its project into `.packages/`, builds `rvnc` if needed,
compiles through the same evaluated target profile used by the language server,
and writes:

- `artifacts/NanoFrameworkTemperature.dll` &ndash; Raven's standard managed output
- `artifacts/NanoFrameworkTemperature.pe` &ndash; the nanoFramework `NFMRK2` image

`NANOFRAMEWORK_PACKAGES_DIR`, `OUTPUT_DIR`, `RAVEN_COMPILER_DLL`,
and `MONO_COMMAND` can override the default tool and output locations.

An ordinary `dotnet build NanoFrameworkTemperature.rvnproj` now also stages the
compact dependency closure and produces
`bin/Debug/netnano1.0/NanoFrameworkTemperature.bin` for direct `nanoff`
deployment. The remaining MVP step is to validate the state transitions and
GPIO output in nanoCLR or on representative hardware.
