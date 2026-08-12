# Getting started with .NET nanoFramework

Raven targets .NET nanoFramework through a normal SDK-style `.rvnproj`. Its
target framework moniker (TFM) is `netnano1.0`. Select that TFM, add ordinary
`PackageReference` items, and build with MSBuild. The
Raven target profile suppresses host .NET references, selects nanoFramework's
core library, and packages the result for nanoCLR.

This is composition rather than a parallel project system. Restore uses normal
`PackageReference` resolution, build and clean use ordinary MSBuild targets,
VS Code invokes the build through `preLaunchTask`, and the resulting artifacts
flow into the official nanoFramework deployment and debugging tools.

Support remains experimental. Check the [target support
matrix](target-platforms.md) before choosing Raven or nanoFramework features for
a production device.

## Prerequisites

Install:

- a .NET SDK supported by Raven;
- the Raven SDK;
- Mono on macOS or Linux, used by the current metadata-processor CLI; and
- the official nanoFramework firmware and deployment tool, `nanoff`.

Install `nanoff` once:

```bash
dotnet tool install --global nanoff
```

Update and inspect an existing installation with:

```bash
dotnet tool update --global nanoff
nanoff --version
nanoff --help
```

## Create a project

This project targets nanoFramework 2.0 preview packages so Raven generics can
run on compatible v2 firmware:

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>netnano1.0</TargetFramework>
    <AssemblyName>Blinky</AssemblyName>
    <OutputType>Exe</OutputType>
  </PropertyGroup>

  <ItemGroup>
    <PackageReference Include="nanoFramework.System.Device.Gpio"
                      Version="2.0.0-preview.18" />
  </ItemGroup>
</Project>
```

Raven implicitly includes `**/*.rvn`. A minimal GPIO program can therefore be
placed directly in `Program.rvn`:

```raven
import System.Device.Gpio.*
import System.Threading.*

func Main() {
    use gpio = GpioController()
    use led = gpio.OpenPin(25, PinMode.Output)

    loop {
        led.Write(PinValue.High)
        Thread.Sleep(500)
        led.Write(PinValue.Low)
        Thread.Sleep(500)
    }
}
```

Pin numbering and electrical connections are board-specific. Pico W and Pico 2
W onboard LEDs use the wireless controller rather than ordinary GPIO 25, so
this exact program requires an external LED on those boards.

## Build and package

Build as an ordinary Raven project:

```bash
dotnet build Blinky.rvnproj --configuration Debug
```

For an executable project, the `netnano1.0` target produces this directory:

```text
bin/Debug/netnano1.0/
  Blinky.dll       Raven managed assembly
  Blinky.pdb       portable source symbols
  Blinky.pe        compact nanoFramework application
  Blinky.pdbx      CLR-to-nanoCLR token and IL map
  Blinky.bin       complete image for nanoff deployment
  mscorlib.pe
  mscorlib.pdbx
  <dependency>.pe
  <dependency>.pdbx
```

The target invokes the official nanoFramework metadata processor, discovers
compact peers for the evaluated package-reference closure, stages the `.pe` and
`.pdbx` files required by the debugger, and combines the core library,
dependencies, and application into the aligned `.bin` accepted by `nanoff`.

Set `NanoFrameworkPackageOnBuild=false` only when another build pipeline owns
metadata conversion. Library projects produce a compact `.pe` by default but do
not create an application `.bin`; set `NanoFrameworkCreateDeploymentImage=true`
only for a library that intentionally acts as the deployment entry assembly.

The build fails when the metadata processor, target core library, or a required
compact dependency is unavailable. Keep nanoFramework package versions mutually
compatible rather than mixing arbitrary stable and preview releases.

## Install compatible firmware

Firmware installation and application deployment are separate operations. The
device must run nanoCLR firmware whose native contracts match the managed
packages. Raven's current samples use nanoFramework 2.0 preview packages, so
they require compatible preview/v2 firmware.

Inspect the firmware targets currently published to the stable and preview
feeds. Treat the result as repository state rather than a permanent board list:

```bash
nanoff --listtargets --platform rpi_pico
nanoff --listtargets --platform rpi_pico --preview
```

The flasher CLI and the firmware repository currently use different names for
the RP2040 boards:

| Board | Published firmware package | `nanoff` CLI alias |
| --- | --- | --- |
| Pico | `RP_PICO_RP2040` | `PICO_RP2040` |
| Pico W | `RP_PICO_W_RP2040` | `PICO_RP2040_W` |
| Pico 2 | Verify the current feed | `PICO2_RP2350` |
| Pico 2 W | Verify the current feed | `PICO2_RP2350_W` |

Tool support for a name does not guarantee that a matching firmware image is
available in the selected feed. Verify the exact board package in the official
[firmware repository](https://cloudsmith.io/~net-nanoframework/repos/nanoframework-images-dev/packages/)
rather than inferring compatibility from a CLI alias alone.

`nanoff` 2.5.162 has two relevant limitations observed during Raven's macOS
hardware validation: its preview lookup can fail to find the published RP2040
packages, and its UF2 application path can count one mounted `RPI-RP2` volume
twice. If either occurs, install the official firmware UF2 directly and use the
nanoCLR wire protocol for application deployment.

For Pico-family firmware updates, hold BOOTSEL while connecting USB. The board
appears as an `RPI-RP2` or `RP2350` mass-storage device. Raven's Pico W hardware
validation used the published `RP_PICO_W_RP2040` package at
`2.0.0-preview.207`. Download and extract the package, then copy `nanoCLR.uf2`
to the mounted board as an ordinary UF2 firmware installation:

```bash
curl -fLO https://dl.cloudsmith.io/public/net-nanoframework/nanoframework-images-dev/raw/names/RP_PICO_W_RP2040/versions/2.0.0-preview.207/RP_PICO_W_RP2040-2.0.0-preview.207.zip
unzip RP_PICO_W_RP2040-2.0.0-preview.207.zip
cp nanoCLR.uf2 /Volumes/RPI-RP2/
```

The final command is the macOS form; on other hosts copy or drag `nanoCLR.uf2`
to the board's BOOTSEL volume. Always select firmware for the exact board and
chip family. Unplug and reconnect normally after the volume disappears or the
copy completes.

Firmware normally changes only when selecting a different target or runtime
version; it does not need to be reflashed for every application build.

## Deploy directly with `nanoff`

The commands in this section use the build output directly and do not depend on
a Raven sample deployment script.

### BOOTSEL/UF2

With the board in BOOTSEL mode, `nanoff` can detect the chip family and convert
the application image without a firmware target name:

```bash
nanoff --platform rpi_pico \
  --deploy \
  --image bin/Debug/netnano1.0/Blinky.bin \
  --uf2deploy
```

`nanoff` performs the UF2 conversion and locates the mounted board. With
`nanoff` 2.5.162 on macOS, prefer the wire-protocol path below if this command
incorrectly reports multiple BOOTSEL devices.

### nanoCLR wire protocol

Once working nanoCLR firmware is running, discover its connection:

```bash
nanoff --listports
nanoff --listdevices -v d
nanoff --nanodevice \
  --serialport /dev/ttyACM0 \
  --devicedetails
```

Deploy without returning to BOOTSEL mode:

```bash
nanoff --nanodevice \
  --serialport /dev/ttyACM0 \
  --deploy \
  --image bin/Debug/netnano1.0/Blinky.bin
```

Replace the example with the reported host port, such as `COM3`,
`/dev/ttyACM0`, or `/dev/tty.usbmodem...`. Automatic `--listdevices` discovery
can miss a Pico that responds when its port is supplied explicitly. A debugger
or serial monitor can hold the same port open, so close it before deployment.
Add `-v d` or `-v diag` when investigating discovery, firmware compatibility,
or deployment failures.

## Visual Studio Code debugging

The official [.NET nanoFramework VS Code
extension](https://marketplace.visualstudio.com/items?itemName=nanoframework.vscode-nanoframework)
provides the device picker, managed debugger interface, and
`nanoframework` Debug Adapter Protocol implementation. It consumes the `.pe`,
`.pdbx`, and portable `.pdb` files produced by Raven's build. Raven's portable
PDB records authored `.rvn` paths, while the Raven VS Code extension enables
breakpoints in Raven documents.

This is the intended division of responsibility: Raven builds and packages the
`.rvnproj`; the nanoFramework extension communicates with nanoCLR and presents
breakpoints, variables, watches, and call stacks.

The nanoFramework extension's **Build Project** and **Deploy Project** commands
currently discover `.nfproj`, not `.rvnproj`, projects. Build and deploy the
Raven project with the commands above, then attach the debugger with a
`.vscode/launch.json` configuration:

```json
{
  "version": "0.2.0",
  "configurations": [
    {
      "name": "Raven nanoFramework: Attach",
      "type": "nanoframework",
      "request": "attach",
      "device": "/dev/ttyACM0",
      "program": "${workspaceFolder}/bin/Debug/netnano1.0",
      "verbosity": "information"
    }
  ]
}
```

The device must be running nanoCLR rather than sitting in BOOTSEL mode. Set
`verbosity` to `debug` when investigating connection, symbol-loading, or
breakpoint-binding failures.

For a build-and-debug F5 workflow, connect the build through a standard VS Code
task:

```json
{
  "version": "2.0.0",
  "tasks": [
    {
      "label": "Raven nanoFramework: Build",
      "type": "process",
      "command": "dotnet",
      "args": [
        "build",
        "${workspaceFolder}/Blinky.rvnproj",
        "--configuration",
        "Debug"
      ],
      "problemMatcher": "$msCompile",
      "group": "build"
    }
  ]
}
```

Then change the launch configuration to `request: "launch"`, add
`preLaunchTask: "Raven nanoFramework: Build"`, and set
`deployAssemblies: true`. An empty `device` lets the nanoFramework extension
reuse the last device, auto-select the only connected device, or show its device
picker. The checked-in
[`nanoframework-blinky`](https://github.com/marinasundstrom/raven/tree/main/samples/projects/nanoframework-blinky) sample
contains complete `tasks.json` and `launch.json` examples for both launch and
attach.

The current metadata processor writes its token map as JSON while the released
nanoFramework VS Code debugger expects the earlier XML PDBX contract. Raven's
`netnano1.0` packaging target stages a debugger-compatible XML view without
changing the compact application or deployment image.

Connected-device validation currently covers deployment, debugger connection,
symbol loading, and source breakpoint binding for the Blinky application.
Stepping through `loop`, generic frames, unions and patterns, local values, and
exception presentation remain explicitly unverified.

## Future integrated commands

Raven's VS Code extension can later recognize `netnano1.0` and generate this
configuration automatically. It should still expose distinct build, deploy,
launch-and-debug, attach-only, and serial-monitor commands rather than hiding
all device operations behind F5.

Official tooling references:

- [`nanoff` firmware and deployment tool](https://github.com/nanoframework/nanoFirmwareFlasher)
- [nanoFramework managed-debugging guide](https://github.com/nanoframework/nf-VSCodeExtension/blob/main/docs/debugging.md)
