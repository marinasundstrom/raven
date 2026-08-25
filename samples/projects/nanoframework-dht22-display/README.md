# nanoFramework DHT22 display

This isolated Pico W hardware sample reads a DHT22 on GP2 every two seconds
and displays its temperature on a 128x64 SH1106 OLED. It contains no Wi-Fi,
HTTP, or LED status code.

## Wiring

For the three-pin DHT22 module shown in the accompanying hardware setup:

| DHT22 module | Pico W |
| --- | --- |
| `+` | `3V3(OUT)`, physical pin 36 |
| `OUT` | `GP2`, physical pin 4 |
| `-` | `GND`, physical pin 38 |

The three-pin module includes its own pull-up resistor. Do not add another one.

| SH1106 OLED | Pico W |
| --- | --- |
| `VCC` | `3V3(OUT)` |
| `GND` | `GND` |
| `SDA` | `GP4`, physical pin 6 |
| `SCL` | `GP5`, physical pin 7 |

The program uses I2C bus 0 and display address `0x3C`.

## Build and deploy

The build defaults to `Release` because the DHT22 one-wire protocol is
timing-sensitive.

```bash
./build.sh
./deploy.sh /dev/tty.usbmodem1201
```

The OLED initially shows `Temp: --.- C`. A successful reading replaces it with
the measured temperature. A failed reading leaves the placeholder visible and
writes `DHT22: read failed.` to the managed debugger output.
