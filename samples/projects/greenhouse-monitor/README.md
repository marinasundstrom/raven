# Greenhouse Monitor (.rvnproj)

This Raven sample models a small greenhouse-monitoring domain. It loads and
validates sensor telemetry, evaluates each growing zone, and prints an operator
report.

The example demonstrates:

- records for domain data
- unions and pattern matching for states and errors
- `Result` propagation with `?`
- arrays, mutable accumulators, and collection interop
- expression-oriented `if` and string interpolation
- code split across a namespace and multiple source files

Project file:

- `GreenhouseMonitor.rvnproj`

Source files:

- `src/main.rvn` contains the domain model, evaluation rules, and console report
- `src/telemetry.rvn` simulates an external telemetry source and validates its data

## Build

From this folder:

```bash
dotnet build GreenhouseMonitor.rvnproj --property WarningLevel=0
```

## Run

```bash
dotnet bin/Debug/net10.0/GreenhouseMonitor.dll
```
