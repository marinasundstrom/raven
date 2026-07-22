# Unsafe code and interoperability

The `*Type` form declares a native pointer to `Type`. Pointer types are
distinct from by-reference types but interoperate with address-of
expressions: taking the address of a local or field produces an address
handle that implicitly converts to the matching pointer type.

Pointer declarations and pointer-type usage require unsafe mode. Enable
unsafe mode with the compiler's `--unsafe` option; otherwise pointer type
syntax is rejected.

Unsafe context can be enabled either globally (compiler option `--unsafe`)
or locally with `unsafe` contexts:

* `unsafe func Name(...) { ... }` for function/method scope
* `unsafe { ... }` as a statement for block scope
* `unsafe { ... }` as an expression for a value-producing block scope

An unsafe block expression has the same value and type as its inner block
expression. It only enables unsafe operations while binding that block; code
before and after the expression remains in the surrounding unsafe context.

```raven
val value = 42
val pointer: *int = &value
```

## Pointer operations

Pointer operations are also gated by unsafe mode. In unsafe mode:

* `*ptr` dereferences a pointer (or by-reference value) and reads the pointed value.
* `*ptr = value` writes through the pointer.
* `ptr->Member` accesses a member on the pointed-at type and is equivalent to
  `(*ptr).Member`, while preserving direct storage semantics (no implicit struct copy for
  instance member calls/field writes).
* `ptr + n` / `n + ptr` advances a pointer by `n` elements.
* `ptr - n` rewinds a pointer by `n` elements.
* `ptr1 - ptr2` returns the element-distance as `nint` when both pointers share the same element type.
  Arithmetic offsets are scaled by `sizeof(element-type)`.

```raven
var value = 41
let pointer: *int = &value

*pointer = 42
val result = *pointer // 42
```

```raven
struct Holder {
    field Foo: int = 0
}

unsafe func assignThroughPointer() -> int {
    var holder = Holder()
    let pointer: *Holder = &holder
    pointer->Foo = 2
    holder.Foo // 2
}
```

## Pinning managed storage

Raven uses a `use`-scoped pinning form instead of a dedicated `fixed (...) { ... }`
statement. The initializer syntax is:

```raven
unsafe {
    use pointer: *int = fixed &value
}
```

Rules:

* `fixed` is only valid as the initializer of a `use` declaration.
* `fixed` requires an explicit address-of operand written as `&expr`.
* The result type is a native pointer `*T`, where `T` is the addressed storage type.
* The pin lasts for the lifetime of the enclosing `use` scope and is released when that scope exits, including exceptional exits.
* `fixed` is gated by unsafe mode just like other pointer-producing operations.

The explicit `&` keeps address selection separate from pinning: `&expr` identifies the storage, while `fixed` guarantees that storage remains stable for pointer use within the `use` scope.

```raven
unsafe func writeSecond(values: int[]) -> int {
    use pointer = fixed &values[1]
    *pointer = 42
    values[1]
}
```

## Extern declarations

Methods and function statements can be marked `extern` to declare that their
implementation is provided externally (for example via P/Invoke). Extern
declarations do not include a body.

Rules:

* Type members marked `extern` must also be `static`.
* `extern` members/functions cannot declare a block body or expression body.
* P/Invoke declarations are expressed with `[DllImport(...)]` on static extern methods.

```raven
import System.Runtime.InteropServices.*

class Native {
    [DllImport("kernel32", EntryPoint: "GetTickCount")]
    extern static GetTickCount() -> uint;
}
```

## Pass by reference

By-reference types can annotate locals and return values. A local
declared with `&Type` acts as an alias to the underlying storage, so
assignments flow through to the referenced location. Functions may
return by-reference values to expose existing storage to the caller. If
you plan to reassign the alias, declare it with `var` so the reference
itself remains mutable.

Taking the address of a value with `&expression` implicitly produces a
by-reference type when the target binding has no explicit annotation.
Use a pointer annotation to force the result into a native pointer
instead of a managed alias.

```raven
func headSlot(values: int[]) -> &int {
    return &values[0]
}

var numbers: int[] = [10, 20, 30]
var slot = headSlot(numbers)
slot = 42 // numbers[0] is now 42

val value = 0
val alias = &value      // alias : &int
val raw: *int = &value  // raw : *int
```

By-reference returns must point to storage that outlives the callee. Returning
`&local` or `&valueParameter` is rejected because those addresses become invalid
after the function returns.

## `ref`/`out` arguments

Parameters can also be declared with an explicit by-reference type using
`&Type`. That form is valid when the type itself is intentionally part of
the signature. Separately, `ref`, `out`, and `in` declare by-reference
parameter passing at the call boundary and already imply aliasing, so
their parameter types are written as plain `Type`, not `&Type`. For
example, write `ref value: int`, not `ref value: &int`. When a
by-reference parameter is passed **into** a function, it behaves just
like a by-reference local: the callee receives an alias to the caller's
storage and can both read and write through that reference. To mark a
parameter that must be assigned by the callee before returning, place
`out` before the parameter name. Parameters are immutable by default, so
add the `var` modifier when you need to reassign the alias—for example
to satisfy an `out` contract or to reuse a ref parameter as scratch
storage. At call sites, pass the argument with the address operator
`&`. (Exact rules are contextual; the binder enforces that the target is
assignable.)

By-reference locals and fields never use the `out` modifier—`out` is
only meaningful at the call boundary to signal definite assignment
responsibilities between caller and callee. Declaring a local with
`ref`, `out`, and `in` parameters immediately alias existing storage
locations. The caller provides that storage with `&expr`; `out`
requires the callee to assign the aliased storage before returning.

```raven
func TryParse(text: string, out result: int) -> bool { /* ... */ }

var total = 0
if !TryParse(arg, out total) {
    Console.WriteLine("Expected number")
}
```
