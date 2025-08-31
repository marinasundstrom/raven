# Proposal: Type compatibility

> ⚠️ 🧩 This proposal has been partly implemented

This document outlines type compatibility and conversion rules in Raven.

## Object compatibility

Reference types inherit from `System.Object`. Value types such as primitives and [unit](../spec/language-specification.md#unit-type) are structs; they do not derive from `object` but can be boxed when a reference is required.

```raven
let o: object = ()      // box the unit value
let p: object = 42      // box an int
```

## Conversions

The compiler classifies conversions using .NET semantics. Unless stated otherwise, the conversions described here are *implicit* and may occur without an explicit cast. Narrowing conversions or other conversions not listed require an explicit cast.

### Identity conversion

An expression can always convert to its own type.

### Null conversion

`null` converts to any nullable reference or nullable value type.

```raven
let s: string? = null
let o: object = null
```

### Nullable lifting

A non‑nullable value type implicitly converts to its nullable counterpart. The lifted value contains the same underlying representation.

```raven
let x: int? = 3   // int lifts to int?
```

### Numeric conversions

Integral and floating‑point types follow .NET numeric conversion rules. Widening conversions such as `int` to `long` or `double` are implicit; narrowing conversions require an explicit cast.

```raven
let a: double = 3       // int → double
let b: long = 1         // int → long
```

### Reference conversions

Reference types convert to their base types or implemented interfaces.

```raven
class Animal {}
class Dog : Animal {}

let a: Animal = Dog()
```

### Boxing and unboxing

Value types, including `unit`, box to `object` or any interface they implement. An explicit cast unboxes the value.

```raven
let o: object = ()  // box unit
let i: object = 42  // box int
```

### Union conversions

An expression converts to a union type if it converts to at least one branch. The union retains the original runtime representation.

```raven
let u: int | string = 1   // uses int branch
```

### User‑defined conversions

Types may declare `op_Implicit` or `op_Explicit` conversion operators. Implicit operators participate in overload resolution; explicit ones require a cast.

## Overload resolution

During overload resolution each argument must have an implicit conversion to the corresponding parameter type. Candidates are ranked by the quality of those conversions:

1. **Identity** conversions
2. **Numeric** widening conversions
3. **Reference** conversions
4. **Boxing** conversions
5. **User‑defined** conversions

A candidate is preferred if it has a better conversion for at least one argument and no worse conversions for the others. If no candidate is better, the invocation is ambiguous.

Literal arguments convert to their underlying primitive type before ranking. `unit` behaves like any other value type and may be boxed when necessary.

```raven
func foo(x: int) {}
func foo(x: double) {}

foo(1)    // chooses foo(int)
foo(1.0)  // chooses foo(double)
```

## Type literals

Type literals are types (`LiteralTypeSymbol`) that represent a constant value and carry an underlying primitive type. See [target typing](../spec/language-specification.md#target-typing) for how literals interact with context.

Examples:

```raven
42      // literal type: 42 (underlying int/System.Int32 or long/System.Int64 depending on size)
0.2     // literal type: 0.2 (underlying float/System.Single or double/System.Double depending on suffix or size)
true    // literal type: true (underlying bool/System.Boolean)
```

### Conversions

Type literals convert to their underlying type. This is important for overload resolution and variable declarations.

```raven
let x: int = 2 + 3        // literal ints convert to int
let y: double = 3         // literal int converts to double
```

### Arithmetic operations

Arithmetic is permitted on literal types whose underlying type defines the operator.

```raven
let a = 2 + 3             // a : int
let b = 1.0 + 2           // b : double (2 converts to double)
```

### Inside expressions

```raven
func foo(x: int) {}
foo(3)                     // literal 3 converts to int
foo(2 + 3)                 // result of addition converts to int
```

### Variable declarations

#### Explicit type

```raven
let a: true = true
```

Even though unusual, a variable can have a literal type.

#### Implicit type

When a declaration relies on inference, the literal's underlying type becomes the variable's type. See [type inference](../spec/language-specification.md#type-inference) for details.

```raven
let a = true      // a : bool
let b = 0.2       // b : double
```

## Terminology

* **literal type** - the type representation or symbol of a literal or constant value
* **literal type expression** - the expression that yields a literal or constant value

