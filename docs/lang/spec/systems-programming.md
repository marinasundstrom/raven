# Systems programming and memory efficiency

Most Raven programs do not need explicit lifetime or storage management. Start
with ordinary values, arrays, collections, classes, records, and functions.
The features in this section are specialized tools for native interoperability,
allocation-sensitive code, and APIs that expose managed memory directly.

Use these features when their constraints are justified by profiling or an
interop contract:

* [Spans and stack allocation](spans-and-memory.md) describes `Span<T>`,
  `ReadOnlySpan<T>`, their conversions, and stack-backed buffers.
* [Ref structs and ref safety](ref-structs-and-ref-safety.md) describes
  ref-like types, managed references, `scoped`, escape restrictions, and the
  `allows ref struct` anti-constraint.
* [Unsafe code and interoperability](unsafe-code-and-interop.md) describes raw
  pointers, address operations, native calls, and pinning managed storage.

These constructs intentionally have stricter rules than ordinary Raven values.
The compiler prevents stack-backed or lifetime-restricted references from being
captured, stored, suspended, or returned into a context that may outlive them.
Those rules preserve memory safety while retaining CLR and C# interoperability.

> ℹ️ **Guidance:** Prefer ordinary Raven APIs in general-purpose application
> code. Introduce these constructs at narrow interop or performance boundaries,
> and keep lifetime-sensitive code localized.
