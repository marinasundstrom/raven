# Proposal: Type compatibility

> ⚠️ 🧩 This proposal has been partly implemented

This document outlines type compatibility

## 

All types are derived from and convertible to `Object`.

## Overload resolution

Overload resolution work like in .NET.

`Unit` participates as a struct, thus is convertible and boxable as `Object`.

## Type literals

Type literals are types (Literal types, `LiteralTypeSymbol`) in the that contains a constant value, and it has an underlying type.

Examples:

```raven
42 // Literal type: 42 (underlying type: int/System.Int or System.Int64 - depending on size)

0.2 // Literal type: 0.2 (underlying type: int/System.Float or double/System.Double - depending on size)

true // Literal type: true (underlying type: bool/System.Boolean)
```

### Conversions

Type literals are convertible to their underlying type. And that is important in overload resolution.

### Arithmetic operations

This applies to literal types with an underlying type that supports arithmetic operations and have addition implemented.

```raven
let x = 2 + 3 // x: int
```

Inside of expressions:

```raven
foo(3) // 2 + 3 is treated as literal type 3, and convertible to target int
foo(2 + 3) // 2 + 3 is treated as type literal, and convertible to target int

func foo(x: int) {}
```

```raven
let x : int = 2
let sum = x + 3 // int + literal type 3  -> int
```

### Variable declarations

#### Explicit type

This is valid:

```raven
let a : true = true
```

Even if not useful.

#### Implicit type

When you are assigning a type literal expression in a implicitly-typed variable declaration (with `let` or `var`), the type will be the underlying type.

```raven
let a = true // int
let b = true // bool
```


## Terminology

* **literal type** - the type representation, or symbol, of a literal or constant value
* **literal type expression** - the actual expression representing a literal or constant value