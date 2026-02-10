# Raven type system

Raven is a statically typed language whose types correspond directly to CLR types. The compiler uses .NET type symbols so that every Raven type has a concrete runtime representation. Conceptually, every CLR type (including structs and other value types) behaves as an object in Raven: value types keep their value semantics, but they participate uniformly in member lookup, generics, and unions through boxing when necessary. Nullability is explicit for both reference and value types, so `?` and `null` are treated consistently regardless of runtime representation.

## Primitive types

| Raven keyword | .NET type | Notes |
| --- | --- | --- |
| `sbyte` | `System.SByte` | 8-bit signed integer |
| `byte` | `System.Byte` | 8-bit unsigned integer |
| `short` | `System.Int16` | 16-bit signed integer |
| `ushort` | `System.UInt16` | 16-bit unsigned integer |
| `int` | `System.Int32` | 32-bit signed integer |
| `uint` | `System.UInt32` | 32-bit unsigned integer |
| `long` | `System.Int64` | 64-bit signed integer |
| `ulong` | `System.UInt64` | 64-bit unsigned integer |
| `nint` | `System.IntPtr` | native-sized signed integer |
| `nuint` | `System.UIntPtr` | native-sized unsigned integer |
| `float` | `System.Single` | 32-bit floating point |
| `double` | `System.Double` | 64-bit floating point |
| `decimal` | `System.Decimal` | 128-bit decimal floating point |
| `string` | `System.String` | UTF-16 sequence of characters |
| `object` | `System.Object` | base type of all .NET reference types |
| `bool` | `System.Boolean` | logical true/false |
| `char` | `System.Char` | UTF-16 code unit |
| `unit` | `System.Unit` | single value `()` representing "no result" |
| `null` | *(null literal)* | inhabits any nullable reference type |

The table lists the built-in keywords that map directly to CLR types. Additional
CLI types can be referenced using their fully qualified names or through
aliases. Raven emits a `System.Unit` struct in the generated assembly so the
`unit` type has a concrete runtime representation and can flow through generics
or tuples. The `null` entry represents the literal value rather than a standalone
type; it may inhabit any nullable reference type and the nullable forms of value
types.

The primitive set defines the building blocks for all other types. `unit` is
singleton-valued and participates in equality, generics, tuples, and unions like
any other value type. The `null` literal is not a separate type; it flows only to
nullable locations and never satisfies non-nullable parameters or bindings.

## Literal types

Numeric, string, character, and boolean literals may appear as their own types.
A literal type represents exactly that value and carries an underlying
primitive type—`1` has underlying type `int` while `"hi"` has underlying type
`string`. Literal types are considered subset types of their underlying primitive,
so every value of a literal type is also a value of that primitive type. Literal
expressions are given these singleton types. These singleton types act as
value-level constraints, most often used as branches in union
types or other constructs that restrict a value to specific constants.

```raven
val value: "yes" | "no" = "yes"

alias Switch = "yes" | "no"
val value: Switch = "yes"

val literalInt: int = 2      // literal widens to its underlying type
```

Supported literal types are:

| Literal example        | Underlying type | Notes                     |
|------------------------|-----------------|---------------------------|
| `true`, `false`        | `bool`          | boolean constants         |
| `'a'`                  | `char`          | single UTF-16 code unit   |
| `"hi"`                | `string`        | sequence of characters    |
| `1`                    | `int`           | 32-bit signed integer     |
| `4_000_000_000`        | `long`          | promoted when `int` overflows |
| `3.14`                 | `double`        | default floating literal  |
| `3.14f`                | `float`         | `f` or `F` suffix selects `float` |

Literal types implicitly convert to their underlying type and then follow the
normal conversion rules of that type. This allows `1` to widen to `double` or
`"hi"` to be used wherever a `string` is expected.

Numeric primitive keywords map directly to CLR numeric types, including `sbyte`,
`byte`, `short`, `ushort`, `int`, `uint`, `long`, `ulong`, `nint`, `nuint`,
`float`, `double`, and `decimal`.

When a literal is assigned to a target whose type is inferred—such as a
variable declaration without an explicit type annotation—the literal widens to
its underlying primitive type. When inference gathers multiple results into a
union (for example, via conditional branches), it normalizes the members so the
union only reports distinct possibilities. Literal members collapse into their
underlying type when a non-literal of that type also flows to the location,
while disjoint literal values remain literal to preserve the precise set of
constants.

```raven
val yes: "yes" = "yes"
val one: 1 = 1
val two: int = one      // implicit conversion to int
val d: double = one     // underlying int widens to double
val inferred = 1        // inferred int, literal type is widened
```

## Composite and derived types

### Arrays

`T[]` becomes `System.Array` with element type `T`. Array element types preserve
their nullability and generic arguments, and indexing uses the CLI's
single-dimensional, zero-based representation (`System.Array.CreateInstance` with
lower bound 0). Multidimensional arrays follow the underlying CLI semantics but
use explicit syntax such as `T[,]` when supported by the grammar.

### Tuples

`(T1, T2, ...)` map to `System.ValueTuple<T1, T2, ...>`.

### Function types

Function types provide a delegate-like type literal. The syntax mirrors a lambda
signature: write the parameter types inside parentheses, then `->`, then the
return type. A single parameter may omit its parentheses, while zero parameters
use the empty tuple `()`.

```raven
val logger: string -> unit
val reducer: (int, int) -> int
val factory: () -> Task<string>
```

In a function parameter:

```raven
func do(op: (int, int) -> int) -> int {
  return op(2, 3)
}
```

The compiler resolves a function type to an existing delegate declaration when a
matching signature is available. This includes the .NET `Func<>`/`Action<>`
families as well as user-defined delegates. When no suitable delegate exists,
the compiler synthesizes an internal delegate whose parameter and return types
match the function type literal. These synthesized delegates participate in
metadata emission so that consumers written in C# or other .NET languages can
invoke them normally.

Nested arrows associate to the right: `int -> string -> bool` means a delegate
that accepts an `int` and returns another delegate of type `string -> bool`.
Return types may be any Raven type, including unions. Function types themselves
may appear anywhere a normal type is expected—alias declarations, parameter
annotations, local bindings, generics, and so on.

### Nullable values

Appending `?` creates a nullable type. Raven does not assume reference types are
nullable by default; `?` is required to permit `null`, just as it is for value
types. Value types are emitted as
`System.Nullable<T>` while reference types use C#'s nullable metadata. Nullable
types are distinct from their non-nullable counterparts for purposes of type
identity and overload resolution. The compiler treats `T?` as accepting both
`T` and `null`, while plain `T` rejects `null` unless the target is widened to a
nullable form (for example, when joining flow branches into a union). Nullable
value types lift member access through the underlying `Nullable<T>` API; nullable
reference types retain the same runtime representation as their non-nullable
form but influence static flow analysis and conversion rules.

#### Strict null checks and flow narrowing

Raven treats `is null` and `is not null` as the strict null-check forms for
flow analysis. These forms always participate in nullability narrowing.

`== null` and `!= null` are also valid, but narrowing is only applied when the
comparison binds to built-in null comparison semantics. If overload resolution
selects a user-defined equality operator, Raven reports a warning and does not
narrow nullability in that branch.

Warning message:

> This comparison may call a custom equality operator, so nullability isn’t
> narrowed. Use `is null` or `is not null` for a strict check.

### Union types

`A | B` represents a value that may be either type. Each branch retains its own
CLR representation; the union records the set of possibilities and supplies a
shared view when one is needed. Raven determines that common view using these
rules:

1. Flatten nested unions and unwrap aliases or literal types to their underlying
   CLR types.
2. Ignore branches whose type kind is `null` while searching for a base type.
3. Walk each remaining branch's inheritance chain (including nullable wrappers)
   and intersect the results to find the most-derived shared base class.
4. Fall back to `System.Object` when no stricter relationship exists. When the
   union also contains `null`, the resulting base behaves as nullable (for
   example, `object?`).

Member lookup on a union delegates to this computed base type, so members defined
on a shared base class remain available. Value-type branches are boxed when the
common denominator is a reference type.

```raven
val union: int | string = "foo"
Console.WriteLine(union.ToString()) // members from the common base type are available
```

Common use cases include mixing unrelated primitives, modeling optional values,
or constraining a value to specific literals:

```raven
val a: int | string = "2"   // either an int or a string
val b: string | null = null // optional string (converts to `string?` when required)
val c: "yes" | "no" = "yes" // constrained to specific constants
```

To model absence explicitly, Raven recommends the **Option union** defined in
`src/Raven.Core/Option.rav` (`System.Option<T>`). It behaves like a `T | null`
union for both reference and value types and implicitly converts to nullable
forms (`T?` or `Nullable<T>`) when interacting with existing .NET APIs that
expect nullable types.

A value is assignable to a union when it can convert to at least one member.
Literal branches are matched by value rather than by type:

```raven
val d: "true" | 1 = 1   // ok
val e: "true" | 1 = 2   // error: Cannot assign '2' to '"true" | 1'
val f: "true" | int = 1 // ok: 1 matches int
```

When a union contains `null` and exactly one other type, it remains a union but
implicitly converts to that type's nullable form when a nullable target is
expected. Attempting to write `string? | int` still produces diagnostic
`RAV0400` because nullable wrappers may not appear explicitly inside unions.
### Generics

Types and functions declare type parameters by appending `<...>` to their
identifier. Each parameter represents a placeholder that is substituted with a
concrete type when the generic is used. The type parameters introduced on a
declaration are in scope for all of its members and may appear anywhere a type
annotation is allowed.

```raven
class Box<T>
{
    public Value: T { get; }

    init(value: T) { Value = value }
}
```

Supplying type arguments between `<` and `>` constructs the desired
instantiation. Raven flows those type arguments through the declaration and
emits regular CLR generic instantiations, so generic Raven code interops with
existing .NET libraries.

```raven
val box = Box<string>("hi")
val copy = box.Value
```

Generic methods use the same syntax. Call sites may provide explicit type
arguments or rely on inference. The compiler infers a type argument when all
arguments (including the expected return type) lead to a single consistent
choice; otherwise, type arguments must be written explicitly.

```raven
func identity<T>(value: T) -> T { value }

val inferred = identity(42)      // infers T = int
val explicit = identity<double>(42)
```

Type parameters optionally declare constraints after a colon. The keywords
`class` and `struct` require reference types or non-nullable value types
respectively. The `class` constraint admits nullable references, while `struct`
excludes `Nullable<T>`. Additional constraints must be nominal types (classes
or interfaces) implemented by the argument. Constraints are comma-separated,
conjunctive, and may appear in any order.

```raven
class Repository<TContext: class, IDisposable>
{
    init(context: TContext) { /* ... */ }
}
```

#### Variance

Interface declarations may annotate their type parameters with variance
modifiers. The keyword `out` marks a parameter as **covariant**, allowing
`Producer<Derived>` to be assigned where `Producer<Base>` is expected. The
keyword `in` marks a parameter as **contravariant**, accepting
`Consumer<Base>` wherever `Consumer<Derived>` is required. Omitting a modifier
keeps the parameter **invariant**, so constructed types such as `Box<string>`
and `Box<object>` remain distinct even when their arguments are related by
inheritance.

```raven
interface Mapper<in TSource, out TResult>
{
    Map(source: TSource) -> TResult
}
```

Variance annotations apply uniformly to source and metadata symbols. Imported
.NET interfaces and delegates continue to surface the CLR's
`GenericParameterAttributes` flags, and Raven-generated symbols report their
`VarianceKind` according to the declared modifiers. These annotations influence
interface implementation checks and conversions so that, for example,
`IEnumerable<string>` is recognised as an implementation of
`IEnumerable<object>`, while `IComparer<object>` satisfies a requirement for
`IComparer<string>`.

#### Array interfaces

Array types surface through `System.Array` but Raven augments the metadata so
that single-dimensional arrays expose the same constructed generic interfaces as
in C#. When an `T[]` symbol is created, the compiler resolves the generic
definitions for `IEnumerable<T>`, `ICollection<T>`, `IList<T>`, and their
read-only counterparts and constructs them using the array's element type. The
resulting interfaces are cached on the `ArrayTypeSymbol`, ensuring they appear
in both `Interfaces` and `AllInterfaces` just like metadata arrays.【F:src/Raven.CodeAnalysis/Symbols/Constructed/ArrayTypeSymbol.cs†L70-L135】

This constructed form lets ordinary interface conversions succeed. Semantic
queries treat `int[]` as implementing `IEnumerable<int>` while still rejecting
incompatible instantiations such as `IEnumerable<string>`, and multi-dimensional
arrays fall back to the non-generic `System.Collections.IEnumerable`
relationship. The behaviour is validated by the existing semantic interface
tests, which check that `SemanticFacts.ImplementsInterface` recognises an array
as satisfying the generic interface contracts for its element type.【F:test/Raven.CodeAnalysis.Tests/Symbols/SemanticFactsTests.cs†L96-L135】

Constraint satisfaction is transitive: substituting a constrained type
parameter for another parameter carries its constraint set. Nullable value
types (`T?`) do not satisfy the `struct` constraint. Violations produce
diagnostics identifying the failing argument and unmet constraint.

## Type identity and aliases

Type aliases provide alternate names for existing types without changing their
identity. The alias participates in overload resolution and conversions exactly
as the underlying type would. Literal types behave similarly: they carry an
underlying primitive and compare equal to that primitive for assignability
checks once a conversion is required. When a union or tuple contains aliases or
literal branches, Raven normalises them during binding so type identity remains
consistent across compilation units.

## Target typing and inference

Many constructs rely on the surrounding context to determine their type. See the
[language specification](language-specification.md#type-inference) for the
complete inference rules. From a type-system perspective, the important effects
are:

- Inferred unions are normalised using the same process described above, so the
  resulting symbol set is stable across recompilations.
- Literal expressions widen to their underlying primitive when no contextual
  type is available but preserve their literal identity inside unions or when
  explicitly annotated.
- Control-flow constructs (such as `if` expressions) contribute their branch
  types to inference, which may introduce unions automatically.

## Delegate inference and method references

Referencing a method as a value produces a delegate type. The inferred type is
driven entirely by the surrounding context: a `let` binding without an
annotation, an assignment, or a method argument supplies the delegate signature
used to select a unique overload. When no compatible delegate type exists,
Raven synthesizes one whose parameters (including `ref`/`out` modifiers) and
return type match the method being referenced. Subsequent method references with
the same signature reuse the synthesized delegate.

Method groups cannot flow into typeless contexts. Writing `val callback =
Logger.Log` produces diagnostic `RAV2201` because no delegate target is
available. Likewise, when multiple overloads remain compatible with the target
delegate (for example, `Action<int>` matching methods that accept `int` or
`long`), diagnostic `RAV2202` is reported and an explicit annotation is required
to disambiguate the binding. If none of the overloads satisfy the delegate's
signature, diagnostic `RAV2203` is emitted.

Instance method references capture the receiver automatically, so evaluating
`self.Increment` stores the current instance along with the method. Invoking the
delegate later observes the same receiver state that existed when the method was
captured. Synthesized delegates participate in metadata emission just like
framework `Func<>`/`Action<>` types, allowing reflection or interop scenarios to
discover the generated `MulticastDelegate` definitions at runtime.

Lambda expressions are implicitly convertible to any compatible delegate type
provided by their target-typed context (assignment, return, or argument
position). Compatibility checks use the delegate's `Invoke` signature. Delegate
types themselves remain distinct; Raven does not perform implicit conversions
between delegate types that merely share the same signature, so converting
between delegate types requires an explicit cast.

## Interoperability

Because Raven reuses .NET types, existing libraries can be consumed seamlessly:

```raven
val ids: Guid[] = [Guid.NewGuid()]
Console.WriteLine(ids[0])
```

## Conversions

Values may convert to other types according to .NET rules. Implicit conversions
include identity, literal types to their underlying primitive type (which also
inherit the underlying type's widening behaviour—`"Foo"` is assignable to
`string`, and `42` may widen to `long`), `null` to any nullable type, lifting
value types to their nullable counterpart, widening numeric conversions,
reference conversions to base types or interfaces, boxing of value types, and
conversions to a matching branch of a union. Narrowing or otherwise unsafe
conversions require an explicit cast. See
[type compatibility](../proposals/type-compatibility.md) for a detailed list of
conversion forms.

Reference conversions include the variance rules encoded on generic interfaces
and delegates. If a referenced interface marks a type parameter as covariant,
`T<Derived>` converts implicitly to `T<Base>`; contravariant parameters invert
the check so `T<Base>` converts to `T<Derived>`. Raven applies these rules when
importing .NET metadata and when compiling Raven source that declares `in` or
`out` variance modifiers, matching the behaviour of the CLR and C#.

When converting **from** a union, each branch must be convertible to the target
type. When converting **to** a union, the source must convert to at least one
branch. These rules also drive overload resolution and assignment diagnostics in
the core compiler.

### Explicit casts

Raven uses C#-style cast syntax for conversions that are not implicit, such as downcasting or numeric narrowing:

```raven
val d = (double)1
val n = (int)3.14
val s = obj as string
```

`(T)expr` performs a runtime-checked cast and throws an `InvalidCastException` if `expr` cannot convert to `T`.
`expr as T` attempts the conversion and yields `null` (or a nullable value type) when it fails.

## Overload resolution

When multiple function overloads are available, Raven selects the candidate whose
parameters require the best implicit conversions. Identity matches are preferred
over numeric widening, which outrank reference or boxing conversions.
User-defined conversions are considered last. An argument of a literal type is an
exact match for a parameter of the same literal type; otherwise the literal is
converted to its underlying primitive type before the ranking is applied. If no
candidate is strictly better, the call is reported as ambiguous.

```raven
val parsed = int.Parse("42") // string literal selects the overload taking string
```

Arguments with union types participate using the union's base type. The compiler
does not test each branch individually; instead, it ranks conversions from the
union's common denominator. This matches IL emission and ensures a union selects
the overload that best matches its shared base type:

```raven
func print(x: object) -> () {}
func print(x: int) -> () {}

val u: int | string = "hi"
print(u) // calls print(object)
```

## Open issues and suggested follow-ups

- **Interface-aware union joins.** The current base-type computation inspects
  only the class hierarchy. As a result, `IFoo | IBar` exposes no shared members
  even when both interfaces derive from a common parent. Consider intersecting
  implemented interfaces and exposing those members when no class-based join
  exists.
- **Canonical form for `T | null`.** Unions that include `null` convert to
  nullable targets but remain unions internally. Evaluate whether the binder
  should normalise `T | null` into `T?` (when `T` is non-nullable) or continue to
  keep the union form and emit clearer diagnostics when a nullable annotation is
  expected.
- **Distribution of `System.Unit`.** Each compiled assembly currently defines its
  own `System.Unit` type. To simplify interop, consider shipping a shared
  reference assembly or mapping `unit` directly onto `System.Void` where the
  runtime allows it.
