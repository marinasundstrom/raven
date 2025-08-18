# Language specification

## Samples

You find them [here](../../../src/Raven.Compiler/samples/).

## File extension

The file extension for source code files is: `.rav`.

## Grammar

An accompanying [EBNF grammar](grammar.ebnf) describes the structural
syntax of Raven. It does not encode contextual rules or the full parsing
process; those details are specified throughout this language
specification.

## Statements

Statements are terminated by newline, or an optional semicolon that may separate multiple statements on one line.

```csharp
let a = 42
let b = 1; b = 3
```

### Top-level statements

Support for top-level statements. Meaning no `Main` method.

```c#
import System

Console.WriteLine("Hello, World!")
```

### `if` statement

```csharp
if x > 3 {
    Console.WriteLine("Hello, World!")
    list[i] = 42
}
```

### `while` statement

```csharp
var i = 0

while i < list.Length {
    let item = list[i]
    Console.WriteLine(item)
    i = i + 1
}
```

## Expressions

### String literals

```c#
let hello = "Hello, "

Console.WriteLine(hello + "World!")

Console.WriteLine("Hello, " + 2)
```

### Array expression and access

```csharp
let list = [1, 42, 3]

let a = list[1]
```

### Function invocation

```csharp
Foo(1, 2)

Console.WriteLine("Test")
```

### Object creation

```rust
let sb = new StringBuilder()
sb.AppendLine("Foo")
```

Generics:

```rust
let list = new List<int>()
list.Add(2)
```

### Tuple

```csharp
let tuple = (a: 42, b: 2)

System.Console.WriteLine(tuple.a)
System.Console.WriteLine(tuple.Item1)
```

### Block expression

```c#
{
    // Body here
}
```

## Namespace declaration

Each file may define a namespace like so:

```c#
namespace Foo

// Members here
```

### Import directive

```c#
namespace Foo

import System

// Members here
```

### Scoped namespaces

You may define multiple namespaces, even nested ones, in one single file by defining block scopes:

```c#

// Members here

namespace A1
{
    import System
    import System.IO

    // Members here

    namespace B1
    {
        // Members here
    }
}

namespace A.B
{
    // Members here
}
```

The outermost undeclared namespace is the global namespace. Meaning whatever is declared there has no prefixed namespace

## Function declaration

```rust
func Foo(a : int, b : b) -> int 
{
    // Body here
}
```

### `ref` and `out` parameters

Using the the `&` address operator.

```c#
var total = 0

if !int.TryParse(arg, &total) {
    Console.WriteLine("Expected number")
}
```

## Local declaration

### Value binding

Once a value is assigned to a name, it can't be re-assigned.

Implicitly typed, with type inference.

```c#
let x = "Foo"
```

With type annotation.

```c#
let x : int = 2
```

Multiple declarators:

```c#
let a : int = 2, b : string = ""
```

### Variable binding

A name that is variable, meaning that a value can be re-assigned.

Implicitly typed, with type inference.

```c#
let x = "Foo"
```

With type annotation.

```c#
let x : int = 2
```

## Types

### Type annotations

Types are specified as type annotations where they can't implicitly be inferred.

Such as `let` or `var` bindings:

```c#
let a = 2

let b : int = 2
```

Or, function declaration:

```csharp
func add(a : int, b : int) -> int { a + b; }
```

### Union type

Sometimes you want to return a value of multiple possible types, such as either and `int` or a `string`.

This is represented as `int | string`.

```csharp
func test(x : int | string) -> void { /* ... */ }
```

Type unions can be produced by `if` expressions.

```c#
let x = 3
let y = if x > 2 { 40 + w; } else { true; }

// y is inferred to be "int | bool"
```

You can then discriminate the values using patterns:

```c#
if y is int a {
    Console.WriteLine(a)
}
else if y is bool b {
    Console.WriteLine(b)
}
```

Under the hood, type unions are represented as type `object`.

### Enums

```c#
var foo : Grades = .B
foo = .C

enum Grades { A, B, C }
```
