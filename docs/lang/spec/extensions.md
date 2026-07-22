# Extension members

Extensions provide helper members for an existing receiver type without
modifying the original declaration. The `extension` keyword declares this
construct.

An extension declaration is a namespace-level container that targets a
specific type via a `for` clause. Importing the container brings its members
into scope for lookup.

An extension declaration may omit its identifier, in which case the
compiler synthesizes a private, mangled container name. Public extensions or
extensions must declare an explicit identifier so the container can be referenced
and imported by name. Applying `fileprivate` keeps the declaration file-local and
mangles the emitted metadata name even when the source uses an explicit
identifier.

Extensions may declare type parameters and generic constraints.
These constraints participate in extension resolution: an extension is
applicable only when its type parameters can be inferred and all declared
constraints are satisfied for the receiver type. Extensions whose constraints
are not met are ignored during member lookup.

Examples:

```raven
extension StringExt for string {
    func ToSlug() -> string => self.Trim().ToLowerInvariant().Replace(" ", "-")
}

import MyApp.StringExt.*
val slug = " Hello World ".ToSlug()
```

```raven
extension for string {
    func IsNullOrWhiteSpace() -> bool => self.Trim().Length == 0
}

val empty = "   ".IsNullOrWhiteSpace()
```

```raven
// Constrained extension: applicable only when the receiver satisfies the constraints.
extension ValueSequenceExt<T> for System.Collections.Generic.IEnumerable<T>
    where T: struct {
    func Sum() -> T { /* ... */ }
}

// If the receiver's element type is not a struct, this extension is ignored during lookup.
```

```raven
// Extension property: participates in member lookup like an instance property.
extension ListIntExt for System.Collections.Generic.List<int> {
    var CountPlusOne: int {
        get => self.Count + 1
        set => self.Add(value)
    }
}

import System.Collections.Generic.*
import MyApp.ListIntExt.*

val items = List<int>()
items.Add(1)

val c = items.CountPlusOne      // invokes getter
items.CountPlusOne = 5          // invokes setter
```

```raven
// Static extension members: accessed through the receiver type when no real static member matches.
extension ListStatics for System.Collections.Generic.List<int> {
    static func Empty() -> System.Collections.Generic.List<int> => System.Collections.Generic.List<int>()
    static DefaultCapacity: int { get => 4 }
}

import System.Collections.Generic.*
import MyApp.ListStatics.*

val empty = List<int>.Empty()
val cap = List<int>.DefaultCapacity
```

Each member inside the body is implicitly an extension member for the receiver
type. Members may be function declarations or computed properties. The compiler
synthesizes a `self` parameter whose type matches the receiver and passes it as
the first argument whenever the member is invoked. The `self` parameter behaves
like a `val` binding: it cannot be reassigned but may be used to access members
or forwarded to other calls. Extension members default to `public`
accessibility and may be marked `internal` to restrict their visibility; other
modifiers are rejected. As a result, extensions cannot declare `protected` or
`private` members.

### Extension methods


Extension methods add callable helpers to the receiver type. They are declared
inside an `extension` container as function members:

```raven
extension StringExt for string {
    func ToSlug() -> string {
        // inside the body, `self` is a synthesized parameter of type string
        return self.Trim().ToLowerInvariant().Replace(" ", "-")
    }
}
```

The receiver parameter determines which expressions may invoke the extension.
Additional parameters follow the ordinary parameter rules: they may be generic,
optional, `params`, or accept lambdas. Generic receiver parameters are
substituted during method type inference, so helpers like `Where<T>(this
IEnumerable<T>, Func<T, bool>)` become available to Raven code as soon as the
appropriate namespace is imported.

#### Partial explicit type arguments

When calling a generic method (including extension methods), Raven permits supplying **only the trailing type arguments** that cannot be inferred.

* The receiver's type is inferred from the call-site receiver expression.
* Any remaining type parameters are inferred from the provided arguments.
* Explicit type arguments provided at the call site are **right-aligned** with the method's type parameter list.

This mirrors the common C# ergonomics for extension calls where the receiver type is already known.

```raven
import System.Console.*
import System.Collections.Generic.*

val items = List<int>()
items.Add(1)
items.Add(2)

extension MyEnumerableExt<T> for System.Collections.Generic.IEnumerable<T> {
    func CountItems<B>(arg: T) -> B {
        return default(B)
    }
}

// T is inferred from the receiver (`IEnumerable<int>`), while B is specified explicitly.
val count2 = items.CountItems<double>(2)
WriteLine(count2)

// The fully specified form is still valid.
val count3 = items.CountItems<int, double>(2)
WriteLine(count3)
```

If the supplied explicit type arguments do not match the method's trailing type parameters (or if inference cannot determine the remaining parameters), overload resolution fails and a diagnostic is reported.

Extension methods participate in lookup through the same `import` mechanism used
for types. Importing a namespace brings every extension method declared on the
static types within that namespace into scope. Importing a specific static type
exposes only the extensions declared on that type. Metadata extensions contained
in referenced assemblies, such as `System.Linq.Enumerable`, and Raven-authored
extensions are surfaced uniformly by binding, so source and metadata callers see
the same candidates.【F:src/Raven.CodeAnalysis/Binder/NamespaceBinder.cs†L33-L61】

Extension classification is determined per member rather than per container.
This matters for mixed extension containers and imported metadata. A single
extension container may therefore contribute both:

* classic instance extension members, which participate in instance-style
  extension lookup and behave like ordinary extension methods/properties, and
* static extension members, which participate only in static member lookup on
  the receiver type.

Invoking an extension uses method-style member access. When `expr.Member(...)`
fails to resolve to an instance member, Raven gathers the in-scope extension
methods whose receiver parameter is compatible with `expr`. Eligible extensions
join overload resolution alongside instance members; if both an instance member
and an extension are applicable, the instance member wins. Once overload
resolution selects an extension, the compiler rewrites the invocation to pass the
receiver as the leading argument to the static method before lowering, so the
generated IL matches the direct static-call form.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L1946-L2001】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/Lowerer.Invocation.cs†L8-L29】

Because extension methods are ordinary `static` methods, accessibility rules and
diagnostics mirror those for other members. Missing imports or inaccessible
extensions produce the same `RAV1xxx` diagnostics emitted for ordinary method
lookups. Lambdas supplied to extension method parameters participate in the same
delegate inference as other calls; Raven replays the lambda for each candidate
signature until one succeeds.

### Extension properties

Extension properties participate in member lookup alongside methods. When a
property access such as `expr.Member` fails to bind to an instance property,
Raven considers imported extension properties whose synthesized `self`
parameter can accept the receiver. Successful binding rewrites the access to
call the accessor extension method (for example, `get_Member(self)`), and
assignments translate to setter invocations that pass both `self` and the
assigned value. Overload resolution prefers instance properties over
extensions, mirroring the method rules. Extension properties are accessor-only:
they cannot declare backing storage, and both accessors must be implemented
with bodies or expression clauses.

```raven
extension ListExt for List<int> {
    var CountPlusOne: int {
        get => self.Count + 1
        set => self.Add(value)
    }
}
```

### Static extension members

Extensions may also declare `static` methods and properties. Static extension
members are associated with the receiver type and are accessed through static
member lookup (`Type.Member`) or by importing the target type (`import
Type.*`). When binding a static member access, the compiler first resolves real
static members on the type; if no match is found, it searches in-scope extension
containers whose receiver type is compatible with the target type. Receiver
compatibility follows the same implicit conversion and nullability rules used
for extension methods, so constructed and nested types participate normally.
Static extension members do not synthesize `self` and are emitted as ordinary
static members on the extension container. Binding classifies them separately
from classic extension methods even when they live in the same extension
container. As a result, a mixed container may expose `value.Member(...)`-style
instance extensions and `Type.Member(...)`-style static extensions at the same
time without forcing one model onto the other.

Pipeline syntax is documented in [Pipe expressions](pipe-expressions.md).
