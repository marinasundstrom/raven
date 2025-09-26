# Compiler

The compiler consists of several components including the command-line driver,
[Raven.Compiler](raven-compiler.md), which can compile `.rav` files and emit
debug information for inspection.

For deeper dives into the implementation see:

- [Architecture](architecture/index.md) for the high-level pipeline and major subsystems.
- [Diagnostics](diagnostics.md) for how errors and warnings are produced.
- [Compiler API](api/README.md) for using syntax, symbols, and the new [operations surface](api/operations.md).
