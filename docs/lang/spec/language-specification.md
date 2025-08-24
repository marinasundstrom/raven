> ⚠️ This is a living document that is subject change

# Language specification

## Code samples

You find them [here](../../../src/Raven.Compiler/samples/).

## Proposals

You find proposals for future language features [here](../proposals/).

## File extension

The file extension for source code files is: `.rav`.

> Perhaps in the future we should use `.rvn`.

## Grammar

An accompanying [EBNF grammar](grammar.ebnf) describes the structural
syntax of Raven. **It is non-normative**: it does not encode contextual rules,
disambiguation, or the full parsing process; those details are specified
throughout this language specification.

## Syntax node model

The syntax node model defines the **logical structure** of Raven programs.
It is specified in [Model.xml](../../../src/Raven.CodeAnalysis/Syntax/Model.xml) and
drives code generation of the immutable syntax tree API.

The model describes the set of node kinds and their children.  
The **parser** applies the grammar and contextual rules to construct these nodes.  

In short: the model defines the shape; the parser defines the rules,
as outlined in this specification.

## Statements

Statements are terminated by a **newline**, or by an **optional semicolon** `;`
that may separate multiple statements on one line. Newlines inside
parentheses/brackets/braces do not terminate statements.

```raven
let a = 42
let b = 1; b = 3
```

Control-flow constructs such as `if` and `while` are expressions.
When used for their side effects in statement position, they appear as expression statements.

### Top-level statements

Top-level statements are supported—no `Main` method is required.

```raven
import System.*

Console.WriteLine("Hello, World!")
```

### Expression statements

Any expression can appear as a statement.

> **Note:** Control flow such as `if` and `while` are **expressions**. When used
> on their own line, they form an `ExpressionStatement`.

## Expressions

### String literals

```raven
let hello = "Hello, "
Console.WriteLine(hello + "World!")
Console.WriteLine("Hello, " + 2)
```

### Array literals and element access

```raven
let list = [1, 42, 3]
let a = list[1]
```

### Function invocation

```raven
Foo(1, 2)
Console.WriteLine("Test")
```

Here’s a fixed and polished version of that section for the spec:

### Object creation

Objects are created by **calling the type name** directly, just like any
other method.

```raven
let sb = StringBuilder()
sb.AppendLine("Foo")
````

Generic types work the same way:

```raven
let list = List<int>()
list.Add(2)
```

Raven also supports the `new` keyword for **backwards compatibility** and
for cases where you want to be explicit about creating an object:

```raven
let sb = new StringBuilder()
let list = new List<int>()
```

This way it’s clear that *constructor-as-call* is the default, and `new` is optional/explicit.  

### Tuple expressions and access

Tuples can be **named** or **positional**. Both projections are available.

```raven
let tuple = (a: 42, b: 2)
Console.WriteLine(tuple.a)      // named
Console.WriteLine(tuple.Item1)  // positional
```

### Block expression

A block is an expression; its value is the value of its last expression
(or `void` if none).

```raven
{
    let x = 10
    x + 1
}
```

### `if` expression

```raven
if x > 3 {
    Console.WriteLine("Hello, World!")
    list[i] = 42
}
```

With `else`:

```raven
let res =
    if cond {
        10
    } else {
        20
    }
```

### `while` expression

```raven
var i = 0
while i < list.Length {
    let item = list[i]
    Console.WriteLine(item)
    i = i + 1
}
```

## Namespace declaration

Each file may define a namespace:

```raven
namespace Foo

// Members here
```

### Import directive

```raven
namespace Foo

import System.*
// or import System.Collections.*

// Members here
```

The wildcard may also be applied to a type name to bring its static members
and nested types into scope:

```raven
import System.Math.*

let pi = PI
```

### Scoped namespaces

You may define multiple namespaces (including nested) in one file using
block scopes:

```raven
// Members in the global namespace

namespace A1
{
    import System.*
    import System.IO.*

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

The outermost undeclared namespace is the **global namespace**.

## Functions

```raven
func Foo(a: int, b: int) -> int
{
    a + b
}
```

Arrow bodies are allowed:

```raven
func add(a: int, b: int) -> int => a + b
```

### `ref`/`out` arguments

Raven uses the **address operator** `&` at call sites. (Exact rules are
contextual; the binder enforces that the target is assignable.)

```raven
var total = 0
if !int.TryParse(arg, &total) {
    Console.WriteLine("Expected number")
}
```

## Local declarations

### Value binding (`let`)

A `let` binding is **immutable** (not reassignable). Types are inferred
unless annotated.

```raven
let x = "Foo"
let y: int = 2
let a: int = 2, b: string = ""
```

### Variable binding (`var`)

A `var` binding is **mutable** (reassignable).

```raven
var x = "Foo"
x = "Bar"

var y: int = 2
y = 3
```

## Types

### Type annotations

Use type annotations where inference is insufficient (e.g., for
parameters, some bindings, or return types):

```raven
let a = 2
let b: int = 2

func add(a: int, b: int) -> int { a + b }
```

### Union types

Unions express multiple possible types (e.g., `int | string`).

```raven
func test(x: int | string) -> void { /* ... */ }
```

Unions arise naturally from control flow:

```raven
let x = 3
let y = if x > 2 { 40 + w; } else { true; }
// y is inferred as: int | bool
```

Discrimination via `is`:

```raven
if y is int a {
    Console.WriteLine(a)
}
else if y is bool b {
    Console.WriteLine(b)
}
```

> **Note:** Representation is an implementation detail; conceptually, unions
> are first-class in the type system even if lowered to `object` at runtime.

### Enums

Enum members can be referenced with a **leading dot** when the type is known.

```raven
var grade: Grades = .B
grade = .C

enum Grades { A, B, C }
```

## Members (classes/structs)

Raven supports classes and structs with fields, methods, constructors,
properties, and indexers. Modifiers are C#-like but validated by the
binder (e.g., `abstract` members require an `abstract` type; `override`
requires a virtual base member).

```raven
class Counter
{
    public let Name: string

    private var _value: int = 0

    init(name: string) { Name = name }

    public Value: int {
        get => _value
        private set => _value = value
    }

    // Indexer
    public this[i: int]: int {
        get => _value + i
    }

    public Increment() -> void => _value = _value + 1
}
```

**Notes**

* Fields use `let`/`var` and require `;` after declarators.
* Accessor-level access (e.g., `private set`) is supported.
* Methods/ctors/properties/indexers may use arrow bodies.

## Operators (precedence summary)

Lowest → highest (all left-associative unless noted):

1. Assignment: `=  +=  -=  *=  /=  %=`
2. Null-coalescing: `??`
3. Logical OR: `||`
4. Logical AND: `&&`
5. Equality: `==  !=`
6. Relational: `<  >  <=  >=`
7. Type tests: `is  as` (binds after relational)
8. Additive: `+  -`
9. Multiplicative: `*  /  %`
10. Unary (prefix): `+  -  !`
11. Postfix trailers: call `()`, member `.`, index `[]`

> **Disambiguation notes**
>
> * `(<expr>)` is a **parenthesized expression** unless a comma appears (including trailing), in which case it’s a **tuple**.
> * `<` starts **type arguments** only in a **type context**; elsewhere it’s the less-than operator.
> * The LHS of assignment must be **assignable** (identifier, member access, element access, or tuple deconstruction).