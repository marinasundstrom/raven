# Target platforms

Raven emits ordinary managed CLI assemblies. A target is therefore defined by
the reference assemblies used during compilation, the identity of its core
library, and the tool that turns the emitted assembly into a deployable
artifact. A target does not define a separate Raven language dialect.

## Support levels

| Platform or deployment model | Level | What works | Important limitations |
| --- | --- | --- | --- |
| Managed .NET | Supported | Raven projects compile and run through the .NET SDK using the selected target framework. | The referenced target framework determines the available API surface. |
| .NET Native AOT | Experimental | The greenhouse-monitor sample reproducibly publishes and runs without trim-analysis or AOT-analysis warnings on macOS Arm64, with a Linux x64 CI smoke test and a defined `linux-arm64` path for Linux-based Raspberry Pi devices. | Linux Arm64 execution has not yet been validated on Raspberry Pi hardware, and broader Raven.Core and generated-helper coverage remains. |
| .NET nanoFramework | Experimental | A standard SDK-style Raven project can select `netnano1.0`, restore normal `PackageReference` items, compile against Raven's nanoFramework target profile, and package a compact `NFMRK2` image. The Blinky probe has also been loaded on a Pico WH running matching preview firmware. | The Raven target profile is provisional pending an official SDK-style nanoFramework target. A nanoFramework Raven.Core build, broader runtime coverage, and visible hardware validation remain. |

“Experimental” means that an end-to-end path has run successfully but is not
yet covered across the supported platform matrix. “Investigation” records a
validated integration boundary, not user-ready target support.

## An IoT spectrum

Native AOT and nanoFramework cover different classes of IoT device rather than
competing for the same deployment:

- **Linux-based single-board computers**, including suitable Raspberry Pi
  models, can run full .NET applications published for a Linux Arm runtime
  identifier such as `linux-arm64`. Native AOT can provide a self-contained
  executable, predictable startup, and no requirement for a separately
  installed .NET runtime.
- **Microcontrollers** have much smaller memory and storage budgets and do not
  host a full Linux/.NET environment. nanoFramework supplies nanoCLR and the
  compact deployment format for this category.

This gives Raven a path across both full operating-system edge devices and
small embedded controllers while keeping the language model shared. Native AOT
also remains a general deployment option for command-line tools, services,
containers, and other applications unrelated to IoT.

## Native AOT

Native AOT is a .NET publishing mode, not a separate Raven target framework or
backend. Raven first emits a normal managed assembly. The .NET SDK and IL
compiler then compile that assembly and its dependency closure to native code.
For a Linux-based Raspberry Pi or similar edge computer, the intended path is
to publish for the matching Linux Arm runtime identifier rather than introduce
a Raspberry Pi-specific Raven dialect.

An SDK-style Raven executable can opt into the normal .NET properties:

```xml
<PropertyGroup>
  <PublishAot>true</PublishAot>
  <RuntimeIdentifier>osx-arm64</RuntimeIdentifier>
</PropertyGroup>
```

It can then be published with the standard SDK command:

```bash
dotnet publish App.rvnproj -c Release
```

The current Raven probe uses `net10.0` and `osx-arm64`. It produces a native
Mach-O Arm64 executable and successfully runs the simulated greenhouse monitor,
including its records, unions, exhaustive patterns, asynchronous telemetry, and
collection interop. The compiler driver excludes its host
`System.Private.CoreLib` from copy-local runtime dependencies, allowing the AOT
toolchain to supply the core library for the selected runtime. Raven-generated
records, unions, and union cases implement a small Raven.Core structured-display
marker, so their synthesized formatting helpers can recognize nested Raven
values without reflective method discovery. The verified publish completes
without trim-analysis warnings.

[`scripts/test-native-aot.sh`](https://github.com/marinasundstrom/raven/blob/main/scripts/test-native-aot.sh)
turns that probe into a regression gate. It publishes into an isolated temporary
directory, executes host-compatible output, and fails if the SDK reports a
trim-analysis or AOT-analysis warning. The Native AOT workflow runs this gate as
`linux-x64` for relevant pull requests and changes on `dev` and `main`.

The reproducible entry point is
[`samples/projects/greenhouse-monitor/publish-aot.sh`](https://github.com/marinasundstrom/raven/tree/main/samples/projects/greenhouse-monitor).
It detects supported macOS and Linux host runtime identifiers, publishes into a
RID-specific artifact directory, and can run host-native output as a smoke test.
On a 64-bit Linux Raspberry Pi, `RUN=1 ./publish-aot.sh linux-arm64` is the
intended local publish-and-run path. Native AOT's platform restrictions still
apply: produce Linux Arm64 output on a compatible Linux Arm64 build host rather
than expecting arbitrary cross-OS compilation.

Native AOT currently remains experimental for Raven. The known compiler-owned
work is:

- validate the repeatable `linux-arm64` publish-and-run path on Raspberry Pi
  hardware and extend CI to representative Linux Arm hardware when available;
- audit Raven.Core and generated helpers for trimming and AOT compatibility;
- report target-specific limitations through diagnostics where the compiler can
  identify them reliably.

Native AOT restrictions in the target runtime and application libraries still
apply. Raven support does not make reflection, dynamic loading, or runtime code
generation automatically compatible with AOT.

See Microsoft's [.NET Native AOT deployment
documentation](https://learn.microsoft.com/dotnet/core/deploying/native-aot/)
for the SDK publishing model and runtime limitations. Microsoft's
[runtime-identifier catalog](https://learn.microsoft.com/dotnet/core/rid-catalog)
documents `linux-arm` and `linux-arm64` for Linux distributions on Raspberry Pi
hardware; the correct identifier depends on the device and operating system.

## .NET nanoFramework

For project setup, standard build outputs, firmware selection, direct `nanoff`
deployment, and the VS Code debugger boundary, see [Getting started with
`netnano1.0`](nanoframework.md).

.NET nanoFramework is a distinct managed runtime for constrained
microcontrollers. It uses its own core library and a compact executable format
consumed by nanoCLR. Supporting it is separate from Native AOT even though both
efforts benefit from removing desktop-runtime assumptions from the compiler.

The investigation established the following:

- the nanoFramework core library exposes the `netnano1.0` target surface;
- the inspected core-library package includes generic types and interfaces,
  nullable value types, generic collections, `Span<T>`, and `ReadOnlySpan<T>`;
- Raven's ordinary managed output was accepted by the official nanoFramework
  metadata processor and converted to an `NFMRK2` `.pe` file; and
- after supplying the nanoFramework core library and suppressing the host .NET
  targeting pack, the emitted probe referenced nanoFramework `mscorlib`
  version `2.0.0.0` and no desktop core library; and
- the temperature-state example below compiled against nanoFramework DHT,
  UnitsNet, GPIO, I²C, device-model, and runtime references, then converted to a
  9.9 KiB `NFMRK2` image.

This snapshot was recorded on August 11, 2026 using
`nanoFramework.CoreLibrary` `2.0.0-preview.52` and the metadata processor CLI
`4.0.0-preview.101`. Package details will change as nanoFramework evolves; the
support level above describes Raven's verified integration, not a promise tied
to those particular preview versions. See the nanoFramework documentation for
its [compact PE format](https://docs.nanoframework.net/content/architecture/pe-file/index.html)
and the official [metadata processor](https://github.com/nanoframework/metadata-processor).

The probes validate Raven's general IL-to-metadata shape, explicit reference
closure, core-library retargeting, representative embedded library binding,
union and pattern emission, and the feasibility of using the existing
nanoFramework packaging tool. The Blinky probe additionally establishes that a
matching nanoCLR firmware can load the Raven assembly and its managed closure;
broader runtime and device compatibility remains unproven.

Generics are consequently not treated as a fundamental blocker or as a reason
to create a reduced Raven syntax. The expected library strategy is to compile
Raven.Core conditionally for `netnano1.0`, retaining portable features and
substituting or omitting APIs that nanoFramework does not provide.

Project-system support is a separate concern. nanoFramework's current
managed build pipeline is centered on `.nfproj`: its metadata processor is an
MSBuild post-build stage of that project format, and its editor integrations
provide dedicated build, deploy, and debug commands. It is not the ordinary
`Microsoft.NET.Sdk` target-framework experience used by a modern `.csproj`.
Raven's MVP now provides an SDK-style `.rvnproj` for the same runtime. Normal
`PackageReference` restore selects `netnano1.0` compile assets, and the evaluated
target profile is shared by `rvnc`, MSBuild, and the language server. Raven's
separate `Raven.nanoFramework.props` build asset supplies the target defaults
that the standard SDK does not yet know while the application continues to use
`Microsoft.NET.Sdk`. That asset can become a dedicated MSBuild SDK profile, or
be replaced by an official SDK-style nanoFramework target, without changing the
application's package-reference model. See nanoFramework's
[description of the `.nfproj` metadata-processing stage](https://docs.nanoframework.net/content/architecture/guide-version-checksums.html)
and its [VS Code managed-code workflow](https://docs.nanoframework.net/content/getting-started-guides/getting-started-managed-vscode.html)
for the current ecosystem model.

### Why nanoFramework fits Raven

Embedded devices and .NET nanoFramework are strategic target areas for Raven,
not merely compatibility experiments. Raven's low-ceremony syntax is a strong
fit for applications where the important code should describe the device
rather than repeat framework structure. Plain functions, inferred local types,
and expression-oriented control flow keep small programs small without giving
up static typing.

Raven also makes functional programming patterns practical in embedded code.
Unions can represent the complete set of device states, protocol messages,
sensor outcomes, or recoverable failures. Exhaustive patterns then make every
state transition visible and cause a new case to identify decisions that need
updating. Immutable values, pure transition functions, `Option`, and `Result`
can keep state-machine logic separate from GPIO, networking, storage, and other
effects. This is precisely the kind of explicit state handling that embedded
software benefits from, expressed without requiring a large object hierarchy
or extensive ceremony.

These language benefits do not imply zero runtime cost. Union representation,
allocation, reflection, generic instantiation, and library size must be measured
on representative devices. Target support should preserve the useful language
model while making those costs visible and avoiding unnecessary runtime
dependencies.

### Temperature-monitor MVP probe

For the smallest deploy-first probe, see the
[`nanoframework-blinky` sample](https://github.com/marinasundstrom/raven/tree/main/samples/projects/nanoframework-blinky).
It uses Raven's `loop` construct to alternate a GPIO output with a timed delay
and passes the selected pin through a generic Raven function. The sample pins a
mutually compatible nanoFramework 2.0 preview managed reference closure so the
probe covers generic metadata in addition to GPIO execution.
The Pico W profile has also completed a hardware deployment probe on a Raspberry
Pi Pico WH using `RP_PICO_W_RP2040` nanoCLR `2.0.0-preview.29`; the device loaded
the Raven executable and its matching 2.0 managed reference closure.
Its build profiles conditionally select GPIO 25 for non-wireless Pico and Pico 2
boards, or accept an external LED GPIO for Pico W and Pico 2 W. The scripts
compile, package, assemble the referenced compact assemblies into a deployment
image, and generate a dry-run `nanoff` command. nanoFramework's published
Current `nanoff` tooling recognizes both RP2040 and RP2350 Pico-family targets;
actual firmware availability still depends on the selected stable or preview
feed and must be checked before flashing.

The following example is the current compile-and-package MVP probe for a
nanoFramework application. A DHT sensor reading becomes a closed domain state.
Successful cases retain the measured value, while `SensorUnavailable` prevents
a failed read from masquerading as a real temperature. The pure `Classify`
function is separate from the GPIO effect. The buildable source, pinned package
snapshot, and packaging script live in the
[`nanoframework-temperature` sample](https://github.com/marinasundstrom/raven/tree/main/samples/projects/nanoframework-temperature):

```raven
import Iot.Device.DHTxx.*
import System.Device.Gpio.*
import System.Threading.*

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
        // Treat a missing sensor as a fault instead of silently continuing.
        .SensorUnavailable => alarm.Write(PinValue.High)
        .Comfortable(_) => alarm.Write(PinValue.Low)
        .TooHot(let celsius) => {
            alarm.Write(PinValue.High)

            // A more severe reading produces a faster alarm pulse.
            let pulseMilliseconds = if celsius >= 40.0 { 100 } else { 500 }
            Thread.Sleep(pulseMilliseconds)
            alarm.Write(PinValue.Low)
        }
    }
}

func Main() {
    use sensor = Dht11(26)
    use gpio = GpioController()
    use alarm = gpio.OpenPin(2, PinMode.Output)

    while true {
        ReadTemperature(sensor)
            |> ActOn(alarm: alarm)

        // DHT sensors need time between readings.
        Thread.Sleep(2000)
    }
}
```

This source has passed Raven compilation and nanoFramework metadata conversion;
it has not yet run under nanoCLR or on hardware. Runnable support still depends
on a nanoFramework Raven.Core build and device validation listed below. The
sensor and GPIO calls follow nanoFramework's
[`DHTxx`](https://docs.nanoframework.net/devicesdetails/Dhtxx/README.html) and
[`System.Device.Gpio`](https://docs.nanoframework.net/api/System.Device.Gpio.GpioController.html)
surfaces. Sensor implementations, pin numbering, and electrical requirements
are board-specific. In particular, ESP32 boards use the dedicated DHTxx ESP32
binding; consult the board and sensor documentation before choosing packages or
connecting components.

### Work required for runnable support

Initial emitter and driver groundwork is now present: Raven has a tested
metadata retargeting step that can replace host `System.Private.CoreLib` and
`System.Runtime` type scopes with a supplied target core-library identity such
as nanoFramework's `mscorlib`, without loading target assemblies for execution.
Compiler hosts can select this behavior through `EmitOptions`. `rvnc`
compilation can additionally use `--no-framework-references`,
`--target-core-library`, and repeated `--refs` options to supply an explicit
target closure. The Blinky `.rvnproj` now drives those options from its evaluated
`netnano1.0` project and ordinary package references.

1. Turn the distributable `Raven.nanoFramework.props` preset into a resolvable
   MSBuild SDK profile when Raven's general MSBuild SDK packaging is introduced,
   while preserving the current standard-SDK project shape.
2. Remove remaining compiler setup and emission assumptions that source core types from
   the compiler host's `System.Private.CoreLib`.
3. Ensure metadata loading works from supplied reference images and does not
   require loading target assemblies into the host runtime.
4. Continue making synthesized helpers capability-aware. Union formatting now
   handles a core library without `Type.IsPrimitive` or
   `string.Replace(string, string)`; other helpers still need an API-surface
   audit.
5. Produce a `netnano1.0` Raven.Core variant using conditional library sources
   where APIs differ.
6. Verify assembly references, generics, unions, exceptions, delegates, and
   static initialization in nanoCLR, then deploy and run smoke tests on an
   emulator and representative hardware.
7. Add target-capability diagnostics and document unsupported Raven.Core APIs.

The first three items are general compiler infrastructure. They also improve
cross-target compilation, compiler hosting, WebAssembly/WASI work, and future
managed runtimes rather than introducing nanoFramework-only branches throughout
the binder or emitter.

## Target architecture

The intended separation is:

- **Language and semantic model:** shared across targets.
- **Reference surface:** defines which framework types and APIs can be bound.
- **Core-library identity:** defines fundamental emitted type references such as
  `object`, `string`, and primitive types.
- **Raven libraries:** conditionally compiled when a target lacks an API or
  needs a different implementation.
- **Artifact pipeline:** managed assembly for ordinary .NET, .NET publish for
  Native AOT, and nanoFramework metadata processing for nanoCLR.
- **Capability diagnostics:** explain unavailable target facilities without
  forking the language.

This separation lets target work strengthen the compiler's overall portability
instead of accumulating special cases for each runtime.
