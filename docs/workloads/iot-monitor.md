# Build an IoT monitor with Native AOT

The greenhouse monitor is an application-shaped Raven project for a full .NET
edge device such as a Linux-based Raspberry Pi. It polls simulated telemetry by
default or live CO2, temperature, and humidity from an SCD40/SCD41 over I2C,
validates each snapshot, models operational states with unions, and produces a
changing console report. The same project can publish as a Native AOT binary.

The complete project lives in
[`samples/projects/greenhouse-monitor`](https://github.com/marinasundstrom/raven/tree/main/samples/projects/greenhouse-monitor).

## Project layout

```text
greenhouse-monitor/
├── GreenhouseMonitor.rvnproj
├── publish-aot.sh
└── src/
    ├── GreenhouseMonitor.rvn
    └── telemetry.rvn
```

`GreenhouseMonitor.rvn` owns the entry point, domain records, state unions,
evaluation functions, and report formatting. `telemetry.rvn` owns the device
boundary: its polling interface, simulated and SCD4x implementations, and input
validation. Configuration selects the adapter without changing the domain
model.

## Consume an async telemetry stream

```raven
async func Main() -> Task {
    let telemetry = CreateTelemetrySource()

    await for result in telemetry.Poll(CancellationToken.None) {
        match result {
            Ok(let readings) => PrintReport(BuildReport(readings))
            Error(let error) => PrintError(error)
        }
    }
}
```

The boundary returns `IAsyncEnumerable<Result<SensorReading[], TelemetryError>>`.
That type makes the stream, successful snapshots, and expected device errors
visible without hiding them behind callbacks or exceptions.

## Model operational states

```raven
record SensorReading(
    Zone: string,
    TemperatureCelsius: double,
    HumidityPercent: double,
    CarbonDioxidePpm: int
)

union ZoneHealth {
    case Healthy
    case Attention(reason: string)
    case Critical(reason: string)
}

union TelemetryError {
    case NoReadings
    case InvalidReading(zone: string, reason: string)
    case SensorUnavailable(reason: string)
}
```

Records describe telemetry data. Unions describe the closed states that the
monitor must handle. Pattern matching then keeps report and recovery paths
explicit.

## Build and run

```bash
dotnet build \
  samples/projects/greenhouse-monitor/GreenhouseMonitor.rvnproj \
  --property WarningLevel=0

dotnet \
  samples/projects/greenhouse-monitor/bin/Debug/net10.0/GreenhouseMonitor.dll
```

The simulated source is the default mock. On a Raspberry Pi with an SCD40 or
SCD41 connected to I2C bus 1 at address `0x62`, select live telemetry with:

```bash
GREENHOUSE_TELEMETRY=scd4x \
GREENHOUSE_ZONE=Propagation \
dotnet \
  samples/projects/greenhouse-monitor/bin/Debug/net10.0/GreenhouseMonitor.dll
```

See the sample README for wiring, I2C setup, and troubleshooting details.

## Publish with Native AOT

From the sample directory:

```bash
./publish-aot.sh
```

On a 64-bit Linux Raspberry Pi, publish and run locally with:

```bash
RUN=1 ./publish-aot.sh linux-arm64
```

Native AOT does not support arbitrary cross-OS publication. Produce the Linux
Arm64 executable on a compatible Linux Arm64 build host. See the sample README
for artifact locations and rebuild options.
