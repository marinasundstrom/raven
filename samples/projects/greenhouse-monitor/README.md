# Greenhouse Monitor (.rvnproj)

This Raven sample models a small greenhouse-monitoring domain. It polls a
simulated device for telemetry snapshots, validates each result, evaluates every
growing zone, and prints a changing operator report. It is also Raven's Native
AOT MVP for full .NET edge devices such as Linux-based Raspberry Pi computers.

The example demonstrates:

- records for domain data
- unions and pattern matching for states and errors
- `Result` propagation with `?`
- an `IAsyncEnumerable` polling interface and `await for` consumption
- cancellation propagated across the simulated device boundary
- arrays, mutable accumulators, and collection interop
- expression-oriented `if` and string interpolation
- code split across a namespace and multiple source files

Project file:

- `GreenhouseMonitor.rvnproj`

Source files:

- `src/GreenhouseMonitor.rvn` contains the domain model, evaluation rules, and console report
- `src/telemetry.rvn` defines the polling interface, simulates a device, and
  validates its data

## Build

From this folder:

```bash
dotnet build GreenhouseMonitor.rvnproj --property WarningLevel=0
```

## Run

```bash
dotnet bin/Debug/net10.0/GreenhouseMonitor.dll
```

## Publish with Native AOT

The publish script builds a self-contained native executable for the current
macOS or Linux host by default:

```bash
./publish-aot.sh
```

Set `RUN=1` to publish and immediately run the native executable:

```bash
RUN=1 ./publish-aot.sh
```

Artifacts are written to `artifacts/native-aot/<rid>/`. Set `OUTPUT_DIR` to
choose another location. Set `FORCE_REBUILD=1` when the local Raven compiler or
Raven.Core sources have changed and the repository toolchain needs rebuilding.

### Raspberry Pi and other Linux Arm64 devices

On a 64-bit Linux Raspberry Pi, publish and run locally with:

```bash
RUN=1 ./publish-aot.sh linux-arm64
```

Native AOT does not support arbitrary cross-OS compilation. Produce the
`linux-arm64` executable on a compatible Linux Arm64 build host (including the
Pi itself), then copy the published directory to the device if deployment is
separate from the build. The sample currently uses simulated telemetry so the
same binary can validate Raven's AOT pipeline without attached sensors; a real
device adapter can replace `SimulatedTelemetrySource` without changing the
union-based state model and evaluation functions.

The current publish completes without trim-analysis warnings. Raven-generated
records, unions, and union cases use a Raven.Core structured-display marker so
their synthesized formatting helpers do not need reflective method discovery.
