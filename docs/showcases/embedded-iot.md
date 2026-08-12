# Model temperature states on a microcontroller

This .NET nanoFramework sample shows how Raven's domain modeling fits close to
a hardware boundary. A union represents the sensor states, and one exhaustive
`match` translates those states into GPIO output.

```raven
union TemperatureState {
    case SensorUnavailable
    case Comfortable(celsius: double)
    case TooHot(celsius: double)
}

func ActOn(state: TemperatureState, alarm: GpioPin) {
    match state {
        .SensorUnavailable => alarm.Write(PinValue.High)
        .Comfortable(_) => alarm.Write(PinValue.Low)
        .TooHot(_) => alarm.Write(PinValue.High)
    }
}
```

## What the sample shows

- A missing sensor is a named state rather than an invented numeric reading.
- Each successful state carries its temperature.
- `ActOn` handles the complete state space and keeps the GPIO effect separate
  from sensor classification.
- Raven uses the nanoFramework `GpioPin` and `PinValue` types directly.

The complete sample reads a DHT sensor, owns device resources with `use`, runs
an unconditional device `loop`, and packages the program as an `NFMRK2` image.

Continue with [building embedded IoT applications](../workloads/embedded-iot.md)
for project setup, packaging, hardware boundaries, and links to the temperature
and Pico blinky samples.
