# Build an embedded IoT application

Raven can target [.NET nanoFramework](https://www.nanoframework.net/) for
managed applications on microcontrollers. The temperature-monitor sample reads
a DHT sensor, models the reading as a closed union, uses exhaustive pattern
matching to drive a GPIO alarm, and packages the result as a nanoFramework
`NFMRK2` image.

The complete project lives in
[`samples/projects/nanoframework-temperature`](https://github.com/marinasundstrom/raven/tree/main/samples/projects/nanoframework-temperature).
A smaller [Pico-family blinky sample](https://github.com/marinasundstrom/raven/tree/main/samples/projects/nanoframework-blinky)
demonstrates GPIO output, board profiles, packaging, and deployment.

## Project layout

```text
nanoframework-temperature/
├── NanoFrameworkTemperature.rvnproj
├── Program.rvn
└── build.sh
```

The project targets `netnano1.0` and references the nanoFramework DHT package
through an ordinary `PackageReference`. Raven's nanoFramework target profile
supplies the matching core library and metadata processor.

## Model sensor states

A sensor reading is not always a temperature. The union makes the unavailable
state explicit instead of inventing a value or relying on a nullable number:

```raven
union TemperatureState {
    case SensorUnavailable
    case Comfortable(celsius: double)
    case TooHot(celsius: double)
}

func Classify(celsius: double) -> TemperatureState {
    return if celsius >= 30.0 {
        .TooHot(celsius)
    } else {
        .Comfortable(celsius)
    }
}
```

## Keep decisions separate from device effects

`ReadTemperature` translates the nanoFramework device API into the domain
union. `ActOn` then handles every state and performs the GPIO effect:

```raven
func ReadTemperature(sensor: Dht11) -> TemperatureState {
    let temperature = sensor.Temperature

    return if sensor.IsLastReadSuccessful {
        Classify(temperature.DegreesCelsius)
    } else {
        .SensorUnavailable
    }
}

func ActOn(state: TemperatureState, alarm: GpioPin) {
    match state {
        .SensorUnavailable => alarm.Write(PinValue.High)
        .Comfortable(_) => alarm.Write(PinValue.Low)
        .TooHot(let celsius) => {
            alarm.Write(PinValue.High)

            let delay = if celsius >= 40.0 { 100 } else { 500 }
            Thread.Sleep(delay)
            alarm.Write(PinValue.Low)
        }
    }
}
```

This split keeps classification testable and makes the hardware boundary
obvious. The main loop owns the sensor and GPIO lifetimes with `use`:

```raven
func Main() {
    use sensor = Dht11(26)
    use gpio = GpioController()
    use alarm = gpio.OpenPin(2, PinMode.Output)

    loop {
        ReadTemperature(sensor)
            |> ActOn(alarm: alarm)

        Thread.Sleep(2000)
    }
}
```

## Build and package

Install a .NET SDK and Mono, then run from the sample directory:

```bash
./build.sh
```

The script restores the project, compiles the Raven source, and writes:

- `artifacts/NanoFrameworkTemperature.dll`, the managed assembly
- `artifacts/NanoFrameworkTemperature.pe`, the compact nanoFramework image

An ordinary project build also produces the staged compact dependency closure:

```bash
dotnet build NanoFrameworkTemperature.rvnproj --property WarningLevel=0
```

Packaging proves the compiler-to-nanoFramework pipeline. Running on hardware
also requires compatible nanoCLR firmware and board-specific wiring. Follow
the blinky sample for the current Pico-family deployment and debugger workflow.

For a full .NET edge device with async streams and Native AOT, see the
[IoT monitor guide](iot-monitor.md).
