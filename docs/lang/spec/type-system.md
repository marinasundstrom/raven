# Raven type system

Raven is statically typed, and every Raven type has a concrete CLR runtime
representation.

## Core rules

* Type identity follows CLR type identity unless Raven defines a language-level
  construct such as nullable annotations or function-type syntax.
* Nullability is explicit for both reference and value types.
* `T?` is the canonical nullable form.
* `unit` (`()`) means "no meaningful result"; it is not a nullable-absence
  construct.

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

## Literal expressions

Numeric, string, character, and boolean literals are expressions whose types
follow normal Raven/.NET typing rules (`1` is `int`, `"hi"` is `string`,
`true` is `bool`, and so on). Raven no longer supports literal values in type
position.

Numeric primitive keywords map directly to CLR numeric types, including `sbyte`,
`byte`, `short`, `ushort`, `int`, `uint`, `long`, `ulong`, `nint`, `nuint`,
`float`, `double`, and `decimal`.

When a literal is assigned to a target whose type is inferred, the inferred type
is its standard primitive type.

```raven
val one = 1
val two: int = one
val d: double = one
val inferred = 1
```

## Composite and derived types

### Arrays

Arrays use CLR array runtime shapes, including fixed-length metadata when the
type is written with an explicit length.

#### Concept

* `T[]` is an open array type.
* `T[N]` is a single-dimensional fixed-length array type.
* Array element types preserve nullability and generic arguments.
* Multidimensional arrays follow CLR semantics when the grammar permits forms
  such as `T[,]`.

#### Example

```raven
val open: int[] = [1, 2, 3]
val fixed: int[3] = [1, 2, 3]
```

#### Rules

* `T[N]` implicitly converts to `T[]`.
* `T[]` does not implicitly convert to `T[N]`.
* `T[N]` converts to `T[M]` only when `N` and `M` are the same length.
* Fixed-length inference is conservative and applies only when the element count
  is directly available from the collection expression.

### Tuples

`(T1, T2, ...)` map to `System.ValueTuple<T1, T2, ...>`.

### Function types

Function types are delegate-like type literals written with arrow syntax.

#### Concept

Write parameter types, then `->`, then the return type. A single parameter may
omit parentheses; zero parameters use `()`.

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

#### Rules

* Function type syntax is arrow-only and does not use the `func` keyword.
* Nested arrows associate to the right: `int -> string -> bool` means
  `int -> (string -> bool)`.
* Function types may appear anywhere a normal type annotation is allowed.
* The compiler binds a function type to an existing compatible delegate when one
  exists, including `Func<>`, `Action<>`, and user-defined delegates.
* If no compatible delegate exists, the compiler synthesizes one with the same
  signature.

### Nullable values

Appending `?` creates a nullable type. Raven does not treat reference types as
nullable by default.

#### Concept

* `T?` accepts both `T` and `null`.
* Plain `T` rejects `null`.
* Nullable value types are emitted as `System.Nullable<T>`.
* Nullable reference types use nullable-reference metadata while keeping the
  same runtime representation as `T`.

#### Example

```raven
val name: string? = null
val count: int? = 1
```

#### Rules

* Nullable and non-nullable forms are distinct for type identity and overload
  resolution.
* `T?` is the canonical nullable form in Raven.
* Postfix `expr!` suppresses nullable checking for a single expression and
  reports warning `RAV0403`.

#### Strict null checks and flow narrowing

Raven treats `is null` and `is not null` as the strict null-check forms for
flow analysis. These forms always participate in nullability narrowing.

`== null` and `!= null` are also valid. Flow narrowing only applies when the
comparison follows built-in null-comparison semantics.

Raven includes an analyzer that recommends replacing `== null`/`!= null` with
`is null`/`is not null` for strict checks. Pointer-like comparisons are exempt.

Warning message:

> ⚠️ This comparison may call a custom equality operator, so nullability isn’t
> narrowed. Use `is null` or `is not null` for a strict check.

### Generics

Types and functions declare type parameters by appending `<...>` to their
identifier. Each parameter represents a placeholder that is substituted with a
concrete type when the generic is used. The type parameters introduced on a
declaration are in scope for all of its members and may appear anywhere a type
annotation is allowed.

```raven
class Box<T>
{
    val Value: T { get; }

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

Constraints also enable static abstract interface member calls on type
parameters. For example, parsing helpers can constrain `T` to `IParsable<T>` and
invoke `T.Parse(...)` directly:

```raven
import System.*

func Parse<T>(text: string) -> T
    where T: IParsable<T>
    => T.Parse(text, null)
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

Function expressions are implicitly convertible to any compatible delegate type
provided by their target-typed context (assignment, return, or argument
position). Compatibility checks use the delegate's `Invoke` signature. Delegate
types themselves remain distinct; Raven does not perform implicit conversions
between delegate types that merely share the same signature, so converting
between delegate types requires an explicit cast.

`func` expressions also support generic signatures and optional local names:
`func<T>(...) where ... { ... }` and `func Fib(...) { ... }`. Generic and named
function expressions are bound as function values first, while projection to a
delegate happens when they flow into parameter or property signatures that
require a delegate type.

For named function expressions, the optional identifier (for example `Fib`) is
scoped to the function-expression body only. It is intended for self-reference
and is not introduced into the enclosing scope.

In Raven-facing type displays, built-in `System.Func`/`System.Action` delegate
shapes are projected as arrow function signatures. User-defined delegates keep
their declared delegate names in displays.

## Interoperability

Because Raven reuses .NET types, existing libraries can be consumed seamlessly:

```raven
val ids: Guid[] = [Guid.NewGuid()]
Console.WriteLine(ids[0])
```

## Conversions

Values may convert to other types according to .NET rules. Implicit conversions
include identity, `null` to any nullable type, lifting
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
User-defined conversions are considered last. If no candidate is strictly better,
the call is reported as ambiguous.

```raven
val parsed = int.Parse("42") // string literal selects the overload taking string
```

Overload ranking follows the normal conversion ladder for the argument and
parameter types in each candidate signature.
