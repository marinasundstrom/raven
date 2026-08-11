# nanoFramework Raspberry Pi Pico-family Blinky

This is Raven's smallest useful .NET nanoFramework MVP sample. It opens an LED
GPIO and uses Raven's unconditional `loop` construct to alternate the output
between high and low every 500 milliseconds.

```raven
loop {
    led.Write(PinValue.High)
    Thread.Sleep(500)
    led.Write(PinValue.Low)
    Thread.Sleep(500)
}
```

Raspberry Pi documents GPIO 25 as the onboard LED connection for Pico and Pico
2. Pico 2 W is different: its LED is attached to the CYW43439 wireless chip and
is not controlled as ordinary GPIO 25.

## Board profiles

The build and deployment scripts recognize `pico`, `pico-w`, `pico2`, and
`pico2-w`. Pico and Pico 2 use GPIO 25 for their onboard LED. The wireless
boards attach their onboard LED to the CYW43 wireless controller instead, so
this deliberately GPIO-only sample requires an external LED and an explicit
`--led-pin` for those profiles. `--led-pin` can also override the LED output for
a non-wireless board.

## Current Pico 2 boundary

Raven can compile and package this managed application for the nanoFramework
assembly surface. At the time of this investigation, nanoFramework's published
Raspberry Pi target documentation covers the RP2040 Pico family, not Pico 2's
RP2350. Running this sample on Pico 2 therefore also requires a compatible
nanoCLR RP2350 firmware port or a future official target. The sample does not
claim that firmware support already exists.

## Build and package

Install a .NET SDK, the `nuget` command-line client, and Mono, then select a
board profile (the default is `pico2`):

```bash
./build.sh
./build.sh --board pico
./build.sh --board pico2
```

Pico and Pico 2 conditionally compile GP25 as the onboard LED. For Pico W and
Pico 2 W, select the GPIO connected to an external LED:

```bash
./build.sh --board pico-w --led-pin 15
./build.sh --board pico2-w --led-pin 15
```

The script restores a deliberately pinned, mutually matching nanoFramework
core/GPIO snapshot, builds `rvnc` if necessary, compiles `Program.rvn`, and
passes the result through the nanoFramework metadata processor. Outputs are:

- `artifacts/<board>/NanoFrameworkBlinky.dll` &ndash; Raven's managed CLI assembly
- `artifacts/<board>/NanoFrameworkBlinky.pe` &ndash; the compact `NFMRK2` image
- `artifacts/<board>/NanoFrameworkBlinky.bin` &ndash; referenced compact assemblies
  and the application, each aligned to four bytes for deployment

This proves compilation and packaging for each profile. Deployment requires
nanoFramework tooling and firmware compatible with the selected board. Pico 2
additionally requires compatible RP2350 firmware. The official deployment
pipeline deploys an application's compact image together with its referenced
class libraries and checks native-component versions against the device
firmware.

`deploy.sh` maps all four profiles to nanoFramework's `rpi_pico` platform and
supports either the wire protocol or Pico BOOTSEL/UF2 deployment. It is a dry
run unless `--execute` is supplied:

```bash
./deploy.sh --board pico --uf2
./deploy.sh --board pico --uf2 --execute
./deploy.sh --board pico --serial-port /dev/ttyACM0 --execute
```

Pico 2 profiles additionally require
`--allow-unpublished-rp2350-firmware`. That flag is an acknowledgement, not a
firmware installer: use it only after installing a compatible RP2350 nanoCLR
build.

References:

- [Raspberry Pi Pico SDK: onboard LED pin](https://www.raspberrypi.com/documentation/microcontrollers/c_sdk.html)
- [nanoFramework's published Raspberry Pi Pico target](https://docs.nanoframework.net/content/rpipico/index.html)

`NANOFRAMEWORK_PACKAGES_DIR`, `OUTPUT_DIR`, `RAVEN_COMPILER_DLL`,
`NUGET_COMMAND`, `MONO_COMMAND`, and `NANOFF_COMMAND` can override the default
locations.
