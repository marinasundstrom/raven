# Proposal: Type compatibility

> ⚠️ 🧩 This proposal has been partly implemented

This document outlines type compatibility and conversion rules in Raven.

## Object compatibility

Reference types inherit from `System.Object`. Value types such as primitives and [unit](../spec/language-specification.md#unit-type) are structs; they do not derive from `object` but can be boxed when a reference is required.

```raven
let o: object = ()      // box the unit value
let p: object = 42      // box an int
```

## Overload resolution

Overload resolution follows .NET conventions. Literal arguments convert to their underlying type when selecting an overload. `unit` participates as a value type and can be boxed like any other struct.

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

