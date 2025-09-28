# Code generation notes

When emitting IL we must resolve CLR metadata types through the same `MetadataLoadContext` used by the compiler.

- Always obtain runtime type symbols via the symbols API or the `CoreAssembly` helpers so they originate from the metadata context hosting the reference assemblies.
- Avoid using `typeof(...)` when you need a `Type` handle in the emitter. `typeof` binds against the compiler host's runtime and the resulting symbol will not match the metadata context.
- If you start from an `ITypeSymbol`, use `GetClrType()` to map the symbol into the `MetadataLoadContext` before passing it to the IL generator.
- Every type, method, and field reference emitted into IL must ultimately resolve through the `MetadataLoadContext`; mixing contexts will produce invalid handles or missing members at runtime.

Following these rules keeps the code generator consistent with the metadata snapshot supplied to `MetadataLoadContext` and ensures we emit IL using the correct reference assemblies.
