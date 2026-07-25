# Spans and stack allocation

Spans are specialized, allocation-free views over contiguous memory.
`System.Span<T>` provides mutable access, while
`System.ReadOnlySpan<T>` provides read-only access. Both support indexed reads,
slicing, and `for` iteration; read-only span indexers are not settable.

The following implicit conversions are available:

* a single-dimensional `T[]` to `Span<T>` or `ReadOnlySpan<T>`;
* `Span<T>` to `ReadOnlySpan<T>`;
* `string` to `ReadOnlySpan<char>`;
* an array, `Span<T>`, or `ReadOnlySpan<T>` to `ReadOnlySpan<U>` when the
  element reference conversion from `T` to `U` is covariant.

Span conversions participate in generic type inference and overload resolution.
When neither target exactly matches the source expression, a span conversion is
preferred over a non-span conversion. Between equivalent mutable and read-only
span targets, `ReadOnlySpan<T>` is preferred.

Collection expressions can directly target either span type:

```raven
val values: System.ReadOnlySpan<int> = [1, 2, 3]
```

`stackalloc T[count]` reserves contiguous storage in the current stack frame.
It naturally produces `Span<T>` and can be explicitly targeted to
`ReadOnlySpan<T>`:

```raven
val buffer = stackalloc byte[256]
val readOnly: System.ReadOnlySpan<byte> = stackalloc byte[16]
```

Stack-allocated storage cannot escape the declaring function. The restriction
follows pointer, span, and read-only-span locals and aliases backed by the
allocation.

`System.Memory<T>` and `System.ReadOnlyMemory<T>` retain their normal .NET
conversion behavior. Their `Span` properties interoperate with the same indexed
and iterative span operations.

See [Ref structs and ref safety](ref-structs-and-ref-safety.md) for the lifetime
rules shared by spans and other ref-like values. Pointer-targeted stack
allocation is covered by
[Unsafe code and interoperability](unsafe-code-and-interop.md#stack-allocation).
