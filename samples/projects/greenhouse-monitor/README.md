# Greenhouse Monitor (.rvnproj)

This Raven sample models a small greenhouse-monitoring domain. It polls either
a simulated device or a Sensirion SCD40/SCD41 sensor connected to a Raspberry
Pi, validates each result, evaluates every growing zone, and prints a changing
operator report. It is also Raven's Native AOT MVP for full .NET edge devices.

The example demonstrates:

- records for domain data
- unions and pattern matching for states and errors
- `Result` propagation with `?`
- an `IAsyncEnumerable` polling interface and `await for` consumption
- a mock and real implementation behind one device boundary
- Raspberry Pi I2C interop through the .NET IoT libraries
- cancellation propagated across the device boundary
- arrays, mutable accumulators, and collection interop
- expression-oriented `if` and string interpolation
- code split across a namespace and multiple source files

Project file:

- `GreenhouseMonitor.rvnproj`

Source files:

- `src/GreenhouseMonitor.rvn` contains the domain model, evaluation rules, and console report
- `src/telemetry.rvn` defines the polling interface, implements the simulated
  and SCD4x sources, and validates their data

## Build

From this folder:

```bash
dotnet build GreenhouseMonitor.rvnproj --property WarningLevel=0
```

## Run

```bash
dotnet bin/Debug/net10.0/GreenhouseMonitor.dll
```

Simulation is the default, so no sensor is required for local development or
tests.

## Read an SCD40 or SCD41 on Raspberry Pi

The SCD4x family supplies the same CO2, temperature, and relative-humidity
values represented by `SensorReading`. Connect an SCD40/SCD41 breakout to the
Pi's I2C bus, following the breakout manufacturer's voltage instructions:

- breakout ground to Pi ground
- breakout SDA to GPIO 2 (physical pin 3)
- breakout SCL to GPIO 3 (physical pin 5)

Enable I2C in `raspi-config`, reboot if prompted, and confirm that the sensor is
visible at its default address `0x62`:

```bash
sudo raspi-config
i2cdetect -y 1
```

Select the hardware adapter with `GREENHOUSE_TELEMETRY=scd4x`. The optional
`GREENHOUSE_ZONE` value becomes the zone name in reports:

```bash
GREENHOUSE_TELEMETRY=scd4x \
GREENHOUSE_ZONE=Propagation \
dotnet bin/Debug/net10.0/GreenhouseMonitor.dll
```

The adapter uses Raspberry Pi I2C bus 1 and reports connection, protocol, and
CRC failures as `TelemetryError.SensorUnavailable`. Stop it with Ctrl+C. Remove
`GREENHOUSE_TELEMETRY` (or give it any value other than `scd4x`) to use the
simulated source again.

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
separate from the build. The same binary defaults to simulated telemetry so it
can validate Raven's AOT pipeline without attached sensors. Set
`GREENHOUSE_TELEMETRY=scd4x` on the Pi to select the real I2C adapter without
changing the union-based state model or evaluation functions.
