# MFRC522 RFID reader

This Raven embedded sample polls an MFRC522 reader over SPI on a Linux-based
single-board computer such as a Raspberry Pi. It validates the card UID,
selects the card, prints its four-byte identifier and capacity code, and drives
an RGB status LED:

- blue: ready and polling
- green: a card was selected successfully
- red: the reader reported a hardware or protocol failure

The sample models the hardware boundary deliberately:

- `Mfrc522Reader` is a class because it owns stateful SPI/GPIO resources and a
  disposal lifecycle.
- byte-valued `Command`, `Register`, and `PiccCommand` enums represent numeric
  constants from the MFRC522 protocol.
- `CardResult<T>` is a union because a poll either succeeds with a value, finds
  no card (an expected state), or fails with a typed `ReaderError`.
- `ReaderError` is a union whose cases carry only the data meaningful for that
  failure.
- `ReaderIndication` is a union interpreted exhaustively by the RGB LED adapter.
- record structs give card identifiers and detected cards immutable,
  domain-specific value shapes instead of passing unrelated arrays and bytes
  through the app.

## Wiring

The defaults use SPI bus 0, chip-select line 0, and GPIO 22 for reset:

| MFRC522 | Raspberry Pi |
| --- | --- |
| SDA / SS | CE0 (GPIO 8) |
| SCK | SCLK (GPIO 11) |
| MOSI | MOSI (GPIO 10) |
| MISO | MISO (GPIO 9) |
| RST | GPIO 22 |
| 3.3V | 3.3V |
| GND | GND |

The RGB LED defaults to a common-cathode device with one current-limiting
resistor per color channel:

| RGB LED | Raspberry Pi |
| --- | --- |
| Red anode | GPIO 17 through a resistor |
| Green anode | GPIO 27 through a resistor |
| Blue anode | GPIO 24 through a resistor |
| Common cathode | GND |

The MFRC522 is a 3.3 V device. Do not power it from 5 V. Enable SPI in the
operating-system configuration before running the sample. For a common-anode
RGB LED, construct `RgbStatusLed` with `activeLow: true` and connect the common
lead to 3.3 V.

## Build and run

From the repository root:

```bash
dotnet build samples/projects/mfrc522-rfid/Mfrc522Rfid.rvnproj --property WarningLevel=0
dotnet run --project samples/projects/mfrc522-rfid/Mfrc522Rfid.rvnproj --property WarningLevel=0
```

The app owns and disposes its SPI and GPIO resources. Press Ctrl+C for an
orderly shutdown.
