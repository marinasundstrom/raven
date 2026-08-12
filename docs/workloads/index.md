# Build applications with Raven

Choose a guide by the kind of application you want to build. Each workload
page explains the general application shape, how it is run or deployed, and
which checked-in samples demonstrate it.

## Available guides

### [Web APIs with ASP.NET Core](web-api.md)

Build a Minimal API with routes, OpenAPI, async handlers, streaming responses,
records, and unions. The guide keeps application composition in `Program.rvn`
and places domain models and handler functions in `Domain.rvn`.

### [File-based applications](file-based-apps.md)

Execute a single Raven source file without a project or separate build step,
pass arguments, use a Unix shebang, and learn when to move into a `.rvnproj`.

### [IoT monitoring and Native AOT](iot-monitor.md)

Poll an asynchronous telemetry stream, model device states and errors, split a
device boundary into its own file, and publish a native executable for an edge
device.

### [Embedded IoT with .NET nanoFramework](embedded-iot.md)

Read a temperature sensor on a microcontroller, model unavailable and alarm
states, drive GPIO output, and package Raven code as a nanoFramework image.

## Workload guide principles

Every guide should answer the same practical questions:

1. What kind of application does this workflow support?
2. When should I choose it instead of another application shape?
3. How is the source organized as the application grows?
4. How do I run, build, publish, or deploy it?
5. Which Raven and .NET facilities matter at this boundary?
6. Which checked-in samples can I study and execute?

The workload area will grow from projects that are already exercised in the
repository. Planned areas include data processing, libraries, background
services, and mixed Raven/C# solutions.

For individual language concepts, use the [language feature guides](../lang/features/index.md).
For compiler and project commands, use the [tooling documentation](../compiler/index.md).
