# nanoFramework Raspberry Pi Pico-family Blinky

This is Raven's smallest useful nanoFramework MVP sample. It opens an LED
GPIO and uses Raven's unconditional `loop` construct to alternate the output
between high and low every 500 milliseconds. The selected pin passes through a
generic Raven function so the sample also exercises nanoFramework's 2.0 generic
metadata and runtime support.

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

## Firmware boundary

Current `nanoff` versions recognize RP2040 and RP2350 Pico-family targets,
including Pico 2. Firmware availability still depends on the selected stable or
preview feed. Because this sample uses nanoFramework 2.0 preview managed
packages, the board must run a matching preview/v2 nanoCLR image.

## Build and package

Install a .NET SDK and Mono, then select a board profile (the default is
`pico2`):

```bash
./build.sh
./build.sh --board pico
./build.sh --board pico2
```

`Program.rvn` declares `extern const LedPin: int = 25`, so Pico and Pico 2 use
GP25 by default. The build script supplies the project's conditional
`RavenConstant` item when an override is requested; the value is converted to
`int` and bound as an ordinary compile-time constant without generating Raven
source. For Pico W and Pico 2 W,
select the GPIO connected to an external LED:

```bash
./build.sh --board pico-w --led-pin 15
./build.sh --board pico2-w --led-pin 15
```

`NanoFrameworkBlinky.rvnproj` uses ordinary `PackageReference` items for a
deliberately pinned, mutually matching nanoFramework 2.0 preview core/GPIO
snapshot. Selecting `netnano1.0` activates Raven's nanoFramework target profile,
which supplies the pinned core library and metadata processor while the project
declares its GPIO dependency normally. The script restores that project with
`dotnet restore`, builds `rvnc`
if necessary, compiles through the normal Raven MSBuild target, and passes the
result through the nanoFramework metadata processor. The compiler and language
server therefore see the same evaluated `netnano1.0` reference surface. The target
firmware must provide the native contracts used by CoreLibrary
`2.0.0-preview.52`, Runtime.Events `2.0.0-preview.13`, and GPIO
`2.0.0-preview.18`. Outputs are:

- `artifacts/<board>/NanoFrameworkBlinky.dll` &ndash; Raven's managed CLI assembly
- `artifacts/<board>/NanoFrameworkBlinky.pe` &ndash; the compact `NFMRK2` image
- `artifacts/<board>/NanoFrameworkBlinky.bin` &ndash; referenced compact assemblies
  and the application, each aligned to four bytes for deployment

This proves compilation and packaging for each profile. Deployment requires
nanoFramework tooling and firmware compatible with the selected board. The
official deployment pipeline deploys an application's compact image together
with its referenced class libraries and checks native-component versions
against the device firmware.

The application remains a stock `Microsoft.NET.Sdk` project. Raven's separate
`Raven.nanoFramework.props` build asset fills the target-framework gap until an
official SDK-style nanoFramework target is available.

`deploy.sh` maps the profiles to the current `PICO_RP2040`, `PICO_RP2040_W`,
`PICO2_RP2350`, and `PICO2_RP2350_W` target names. It supports either the wire
protocol or Pico BOOTSEL/UF2 deployment and is a dry run unless `--execute` is
supplied:

```bash
./deploy.sh --board pico --uf2
./deploy.sh --board pico --uf2 --execute
./deploy.sh --board pico --serial-port /dev/ttyACM0 --execute
```

## Hardware validation

The Pico W profile has been deployed to a Raspberry Pi Pico WH running the
`RP_PICO_W_RP2040` nanoCLR `2.0.0.29` firmware. The device accepted and
loaded `NanoFrameworkBlinky` together with the matching 2.0 managed reference
closure. The deployed entry point includes the closed `SelectLedPin<int>`
generic method and targets external GPIO 15. The onboard Pico WH LED is not part
of this validation because it is attached to the CYW43 wireless controller; a
visible blink requires an external LED and resistor or observation with a logic
probe.

References:

- [Raspberry Pi Pico SDK: onboard LED pin](https://www.raspberrypi.com/documentation/microcontrollers/c_sdk.html)
- [nanoFramework's published Raspberry Pi Pico target](https://docs.nanoframework.net/content/rpipico/index.html)

`NANOFRAMEWORK_PACKAGES_DIR`, `OUTPUT_DIR`, `RAVEN_COMPILER_DLL`,
`MONO_COMMAND`, and `NANOFF_COMMAND` can override the default locations.

For the normal project workflow without the sample deployment wrapper, see
[Getting started with nanoFramework](../../../docs/compiler/nanoframework.md).
It covers standard build output, direct `nanoff` firmware and deployment
commands, and the current VS Code debugger boundary.

When this sample directory is opened as the VS Code workspace, its checked-in
`.vscode/tasks.json` and `.vscode/launch.json` provide **Raven nanoFramework:
Launch and Debug** and **Raven nanoFramework: Attach**. The launch configuration
runs the normal Raven MSBuild task first, lets the official nanoFramework
extension select the connected device, deploys the staged compact assemblies,
and starts its managed debugger. The build task prompts for the LED GPIO so the
same launch configuration covers the onboard Pico/Pico 2 LED and an external
LED on wireless boards.
