# Raven tools

This section explains the tools needed to create, compile, run, and edit Raven
programs. The `rvn` command manages projects and developer workflows, while the
`rvnc` compiler driver compiles `.rav` source files for .NET. The toolchain also
ships the [Raven Core Library](raven-core-library.md), which is referenced by
default.

Start with:

- [Compiler and command-line tools](raven-compiler.md)
- [Target platforms](target-platforms.md)
- [.NET conformance and Raven divergences](architecture/dotnet-conformance-and-divergence.md)
- [Project system](project-system.md)
- [Extend a project](extending-projects.md)
- [VS Code extension](raven-vscode-extension.md)
- [Diagnostics](diagnostics.md)
- [Built-in analyzers](analyzers/built-in.md)
- [Analyzer configuration](analyzers/configuration.md)
- [JSON serialization](json-serialization.md)

Project extensions are covered from a user and library-author perspective.
The [compiler API](api/README.md) remains available as supporting reference for
macro authors, tooling developers, and compiler integrations, but language use
and the standard [Raven libraries](../libraries/index.md) are the primary
documentation paths. Compiler architecture, detailed API designs, and
contributor workflows remain separate from the user manual.
