# Build applications with Raven

Learn Raven through the kind of application you want to build. Each workload
guide starts with a working project, shows how the source is organized, and
explains the Raven choices that matter for that application shape.

## Available guides

### [Web APIs with ASP.NET Core](web-api.md)

Build a Minimal API with routes, OpenAPI, async handlers, streaming responses,
records, and unions. The guide keeps application composition in `Program.rvn`
and places domain models and handler functions in `Domain.rvn`.

### [Command-line applications](command-line.md)

Run a single Raven file with command-line arguments, including a Unix shebang
workflow, and learn when to move from a file into a `.rvnproj`.

### [IoT monitoring and Native AOT](iot-monitor.md)

Poll an asynchronous telemetry stream, model device states and errors, split a
device boundary into its own file, and publish a native executable for an edge
device.

### [Embedded IoT with .NET nanoFramework](embedded-iot.md)

Read a temperature sensor on a microcontroller, model unavailable and alarm
states, drive GPIO output, and package Raven code as a nanoFramework image.

## Workload guide principles

Every guide should answer the same practical questions:

1. What are we building, and which parts of Raven does it demonstrate?
2. What does the project layout look like?
3. Where does application composition belong?
4. Where do domain types and functions belong as the project grows?
5. How do I build, run, and exercise the application?
6. Which parts are ordinary .NET, and which parts are Raven-specific?

The workload area will grow from projects that are already exercised in the
repository. Planned areas include data processing, libraries, background
services, and mixed Raven/C# solutions.

For individual language concepts, use the [language feature guides](../lang/features/index.md).
For compiler and project commands, use the [tooling documentation](../compiler/index.md).
