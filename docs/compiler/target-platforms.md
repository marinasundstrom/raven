# Target platforms

Raven emits ordinary managed CLI assemblies. A target is therefore defined by
the reference assemblies used during compilation, the identity of its core
library, and the tool that turns the emitted assembly into a deployable
artifact. A target does not define a separate Raven language dialect.

## Support levels

| Platform or deployment model | Level | What works | Important limitations |
| --- | --- | --- | --- |
| Managed .NET | Supported | Raven projects compile and run through the .NET SDK using the selected target framework. | The referenced target framework determines the available API surface. |
| .NET Native AOT | Experimental | A Raven console application has been published as a native macOS Arm64 executable and run successfully. | Some synthesized union formatting helpers currently produce trimming warnings because they use reflection. Broader platform and library coverage is still needed. |
| .NET nanoFramework | Investigation | Raven can emit a managed assembly that the nanoFramework metadata processor accepts and converts to its compact `NFMRK2` `.pe` format. | The emitted assembly still names the desktop .NET core library. Raven cannot yet produce a nanoFramework-runnable application or deploy one to a device. |

“Experimental” means that an end-to-end path has run successfully but is not
yet covered across the supported platform matrix. “Investigation” records a
validated integration boundary, not user-ready target support.

## Native AOT

Native AOT is a .NET publishing mode, not a separate Raven target framework or
backend. Raven first emits a normal managed assembly. The .NET SDK and IL
compiler then compile that assembly and its dependency closure to native code.

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

The initial Raven probe used `net10.0` and `osx-arm64`. It produced a native
Mach-O Arm64 executable and successfully ran the Raven Hello World sample. The
compiler driver now excludes its host `System.Private.CoreLib` from copy-local
runtime dependencies, allowing the AOT toolchain to supply the core library for
the selected runtime.

Native AOT currently remains experimental for Raven. The known compiler-owned
work is:

- remove the trimming warnings caused by reflective method lookup in
  synthesized union value formatting;
- add repeatable publish-and-run coverage on representative runtime identifiers;
- audit Raven.Core and generated helpers for trimming and AOT compatibility;
- report target-specific limitations through diagnostics where the compiler can
  identify them reliably.

Native AOT restrictions in the target runtime and application libraries still
apply. Raven support does not make reflection, dynamic loading, or runtime code
generation automatically compatible with AOT.

See Microsoft's [.NET Native AOT deployment
documentation](https://learn.microsoft.com/dotnet/core/deploying/native-aot/)
for the SDK publishing model and runtime limitations.

## .NET nanoFramework

.NET nanoFramework is a distinct managed runtime for constrained devices. It
uses its own core library and a compact executable format consumed by
nanoCLR. Supporting it is separate from Native AOT even though both efforts
benefit from removing desktop-runtime assumptions from the compiler.

The investigation established the following:

- the nanoFramework core library exposes the `netnano1.0` target surface;
- the inspected core-library package includes generic types and interfaces,
  nullable value types, generic collections, `Span<T>`, and `ReadOnlySpan<T>`;
- Raven's ordinary managed output was accepted by the official nanoFramework
  metadata processor and converted to an `NFMRK2` `.pe` file; and
- that converted probe still references `System.Private.CoreLib`, whereas the
  nanoFramework runtime surface uses its own `mscorlib` identity.

This snapshot was recorded on August 11, 2026 using
`nanoFramework.CoreLibrary` `2.0.0-preview.52` and the metadata processor CLI
`4.0.0-preview.101`. Package details will change as nanoFramework evolves; the
support level above describes Raven's verified integration, not a promise tied
to those particular preview versions. See the nanoFramework documentation for
its [compact PE format](https://docs.nanoframework.net/content/architecture/pe-file/index.html)
and the official [metadata processor](https://github.com/nanoframework/metadata-processor).

The probe validates Raven's general IL-to-metadata shape and the feasibility of
using the existing nanoFramework packaging tool. It does not establish runtime
compatibility. The core-library identity mismatch must be fixed before a Raven
program can be considered runnable on nanoCLR.

Generics are consequently not treated as a fundamental blocker or as a reason
to create a reduced Raven syntax. The expected library strategy is to compile
Raven.Core conditionally for `netnano1.0`, retaining portable features and
substituting or omitting APIs that nanoFramework does not provide.

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

### Prospective GPIO example

The following example illustrates the intended Raven experience for a
nanoFramework application. It models the LED as a closed state, keeps the state
transition pure, and isolates the GPIO write at the device boundary:

```raven
import System.Device.Gpio.*
import System.Threading.*

union IndicatorState {
    case Off
    case On
}

func Next(state: IndicatorState) -> IndicatorState {
    return state match {
        .Off => .On
        .On => .Off
    }
}

func WriteState(pin: GpioPin, state: IndicatorState) {
    let value = state match {
        .Off => PinValue.Low
        .On => PinValue.High
    }

    pin.Write(value)
}

func Main() {
    // Pin 2 is the built-in LED on some ESP32 boards. Board mappings vary.
    let ledPinNumber = 2
    use gpio = GpioController()
    use led = gpio.OpenPin(ledPinNumber, PinMode.Output)
    var state: IndicatorState = .Off

    while true {
        WriteState(led, state)
        Thread.Sleep(500)
        state = Next(state)
    }
}
```

This is a design example, not a currently deployable Raven program. Runnable
support depends on the core-library identity, target-reference, Raven.Core, and
packaging work listed below. The GPIO calls follow nanoFramework's
[`System.Device.Gpio`](https://docs.nanoframework.net/api/System.Device.Gpio.GpioController.html)
surface. Pin numbering and electrical requirements are board-specific; consult
the board documentation before connecting external components.

### Work required for runnable support

1. Introduce an explicit target-reference model containing the reference
   closure, core-library identity, target-framework identity, and target
   capabilities.
2. Remove compiler setup and emission assumptions that source core types from
   the compiler host's `System.Private.CoreLib`.
3. Ensure metadata loading works from supplied reference images and does not
   require loading target assemblies into the host runtime.
4. Produce a `netnano1.0` Raven.Core variant using conditional library sources
   where APIs differ.
5. Integrate the nanoFramework metadata processor as a packaging stage after
   Raven emits standard managed IL.
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
