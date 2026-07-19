# Greenhouse Monitor (.rvnproj)

This Raven sample models a small greenhouse-monitoring domain. It polls a
simulated device for telemetry snapshots, validates each result, evaluates every
growing zone, and prints a changing operator report.

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

- `src/main.rvn` contains the domain model, evaluation rules, and console report
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
