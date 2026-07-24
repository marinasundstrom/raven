# Object initialization and copying

Object initializers set named members while an object is being created. A
`with` expression creates a changed copy of an existing value without changing
the original.

## Object initializers

Use an object initializer when several named members or content items need to
be set as part of creating the object.

An **object initializer** is a brace block after a type name or constructor
call. A type name without parentheses selects its parameterless constructor;
an explicit constructor call may supply arguments. The compiler constructs the
instance and then applies initializer entries in source order.

```raven
val window = Window {
    Title = "Main"
    Width = 800
    Height = 600
}
```

Init-only accessors are treated as initializer-only members, so they may be assigned in object initializers:

```raven
class Settings {
    val Theme: string { init; }
    val FontSize: int { init; }
}

val settings = Settings {
    Theme = "Dark"
    FontSize = 14
}
```

Initializer bodies consist of a sequence of **member entries** and **content entries**. A member entry assigns to a writable property, field, or event using `Name <assignment-operator> Expression`. A content entry is a standalone expression; when the initialized type has a mutable `Content` property, the first content entry initializes that property, otherwise content entries are lowered through a compatible instance `Add(T)` method.

* `=` is valid for writable fields/properties.
* Compound assignment operators (for example `+=`) are supported and are evaluated as member compound assignment on the initialized instance.
* Events use `+=` / `-=` in initializer context, matching statement assignment rules.

Event subscription is valid in object initializers:

```raven
val button = Button {
    Clicked += () => WriteLine("clicked")
}
```

Property entries are applied to the newly created instance in source order.

Initializers may follow constructor arguments:

```raven
val person = Person("Ada") {
    Age = 36
}
```

These braces are object-initializer syntax, not function arguments. Pass a
function with ordinary function-expression syntax inside the argument list.
Use `value with { ... }` separately for non-destructive copying.

## Required members and init semantics

Members marked as **required** must be definitely assigned during object construction. A required member may be a field or property. Required members participate in object initializer checking and influence which constructors are considered complete.

```raven
class Person {
    required val Name: string { init; }
    required val Age: int { init; }
}

val p = Person { Name = "Ada", Age = 36 }   // ok
val q = Person { Name = "Ada" }             // error: Age must be set
```

### Declaration rules

* A required **field** must be **mutable**. Declaring `required` on an immutable (`let`) field is an error.
* A required **property** must have an accessible **init** or **set** accessor so that it can be assigned during initialization.
* `required` is not permitted on `const`, `static`, or read-only members.

### Constructors and `SetsRequiredMembers`

A constructor may satisfy required members directly. Such constructors are annotated with the attribute `System.Diagnostics.CodeAnalysis.SetsRequiredMembersAttribute`. When a constructor carries this attribute, the compiler considers all required members to be assigned by that constructor and does not require an object initializer.

```raven
record Person(Name: string, Age: int)
// primary record constructor is treated as [SetsRequiredMembers]

val p = Person("Ada", 36)   // ok
```

`record Name(...)` is shorthand for `record class Name(...)`. Use
`record struct Name(...)` for a value-type record.

Primary-constructor behavior is intentionally split:

1. `class`/`struct`: parameters marked with `val` or `var` are promoted to properties; parameters without a binding keyword are captured in compiler-generated private storage for member access, but are not promoted to public properties. Promoted parameters may include an access modifier (`public`/`internal`/`protected`/`private`) before `val`/`var` to set synthesized property accessibility (default `public`).
2. `record class`/`record struct`: positional parameters define the record's public data shape via synthesized properties and value members. When no binding keyword is present, record parameters are promoted as `val` properties by default. Value-shape synthesis (`Equals`, `GetHashCode`, `Deconstruct`, `ToString`, record copy/with flow, and equality operators) includes only **public** promoted properties; non-public promoted properties are excluded and produce a compiler warning.
3. Constructor calls require invocation syntax (`Foo(...)` or `Foo()`), except
   that `Foo { ... }` is the parameterless object-initializer form. A standalone
   type name (`Foo`) is not a value expression.
4. Member declarations cannot reuse the immediate containing type's name.

For semantic-model queries, unqualified identifier access to those captured/promoted members resolves to the originating primary-constructor parameter symbol.

For constructors **without** this attribute, all required members must be provided by an object initializer at each creation site.

### Object initializer checking

When binding an object creation expression:

1. Collect the set of required members declared on the type and its base types.
2. If the selected constructor has `SetsRequiredMembers`, no further checks are performed.
3. Otherwise, an object initializer must assign **all** required members.
4. Omitting any required member produces a compile-time error.

Assignments in the initializer may target fields or properties with `init`/`set` accessors. Nested initializers and `with` expressions are treated as initializer contexts.

### Inheritance

Required members declared on base types are inherited by derived types and must also be satisfied during construction of the derived type, unless a base constructor is marked with `SetsRequiredMembers`.

### Interaction with `with` expressions

`with` expressions operate in initializer context. Required members may be assigned within a `with` initializer just as in object initializers:

```raven
val p2 = p with { Age = 37 }
```

## With expressions

A **with expression** creates a copy of a value and applies a list of member assignments without mutating the original instance. The syntax is:

```raven
val updated = point with {
    X = 10
    Y = 20
}
```

The receiver expression is evaluated exactly once. Each assignment expression is evaluated left-to-right in source order. If a member is listed more than once, the compiler reports `RAV0241`.

Assignments in a with initializer must target writable instance fields or properties. `init` accessors are permitted because with initializers are treated as initializer contexts (matching object initializer semantics).

With expressions update record values by producing a new record instance while
preserving the original:

```raven
record Point(X: int, Y: int)

val origin = Point { X = 0, Y = 0 }
val moved = origin with { X = 10 }
```

For records, synthesized copy/clone behavior follows the record value shape and therefore includes only public promoted properties; non-public promoted properties are not copied by synthesized record copy semantics.

When binding a with expression, the compiler selects the first applicable strategy in the following order:

1. **Record clone** — Record types clone first, then apply assignments as initializer-style member assignments.
2. **`Update(...)` convention** — An instance method named `Update` whose parameter names correspond to readable members on the receiver. Each parameter receives either the provided assignment value (if present) or the receiver's current member value.
3. **`With(...)` convention** — Same as `Update`, but with an instance method named `With`.
4. **`WithX(...)` chaining** — For each assignment `X = expr`, invoke a single-parameter method named `WithX`. Methods are invoked in source order, and each invocation receives the assignment's value expression.
5. **Clone/copy fallback** — Use a parameterless `Clone()` method or a copy constructor (one parameter of the receiver type) to create a copy, then apply assignments as initializer-style member assignments.

If none of these conventions apply, the compiler reports `RAV0240` to indicate the type does not support `with` expressions.
