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

## Unit type

Raven has no `void` type. The absence of a meaningful value is represented by the
`unit` type, which has exactly one value written `()`. The type itself may be
spelled `unit` or `()`. Functions without an explicit return type implicitly
return `unit`. When interacting with .NET, methods that return `void` are
projected as returning `unit`, and Raven's `unit` emits as `void` unless the
value is observed. The `unit` type is a value type (struct). Because `unit` is a
real type, it participates in generics, tuples, and unions like any other type.

## Statements

Statements are terminated by a **newline**, or by an **optional semicolon** `;`
that may separate multiple statements on one line. Newlines inside
parentheses/brackets/braces do not terminate statements.

```raven
let a = 42
let b = 1; b = 3
```

### Variable bindings

`let` introduces an immutable binding while `var` introduces a mutable one. A binding
may declare its type explicitly or rely on the compiler to infer it from the
initializer expression.

```raven
let answer = 42        // inferred int
var name = "Alice"   // inferred string, mutable
let count: long = 0    // explicit type
```

If the type annotation is omitted, an initializer is required so the compiler can
determine the variable's type. With an explicit type, the initializer may be
omitted.

Control-flow constructs such as `if`, `while`, and `for` are expressions.
When used for their side effects in statement position, they appear as expression statements.

### File-scope code

File-scope code is supported—no `Main` function is required.

```raven
import System.*
alias print = System.Console.WriteLine

sayHello()

func sayHello() {
    print("Hello, World!")
}
```

### Expression statements

Any expression can appear as a statement.

> **Note:** Control flow such as `if`, `while`, and `for` are **expressions**. When used
> on their own line, they form an `ExpressionStatement`.

### Return statements

The `return` keyword exits a function, lambda, or property accessor. Because control-flow constructs are expressions, using `return` inside an expression that itself produces a value is not allowed. Explicit `return` statements may appear only in statement positions, such as within a function body or as their own expression statement. When a `return` occurs in a value context—for example, within an `if` expression assigned to a variable—the compiler reports diagnostic `RAV1900` and the block should rely on an implicit return instead.

A `return` statement may omit its expression when the surrounding function or accessor returns `unit` (projected as `void` in IL). This is equivalent to returning the `()` value explicitly.

```raven
func choose(flag: bool) -> int | () {
    if flag {
        42            // implicit return
    } else {
        ()             // implicit return
    }
}

if flag {
    return 42          // allowed: expression used as a statement
}

func log(msg: string) {
    Console.WriteLine(msg)
    return            // equivalent to returning ()
}
```

## Expressions

### Target typing

Many expressions rely on the type expected by their context, called the **target type**.
For example, the enum shorthand `.B` in `var grade: Grades = .B` uses the declared type
`Grades` to resolve the member. Numeric literals and `null` similarly adapt to their
target types. Type inference for `let` and `var` bindings uses this mechanism to
determine the variable's type from its initializer.

### Type inference

When an expression or declaration omits an explicit type, Raven infers one from
the expression. If multiple different types can flow to a location—through
conditional branches or early `return` statements—the inferred result becomes a
**union** of those types. The compiler does not collapse distinct types to their
nearest common base; returning `Dog` and `Cat` infers `Dog | Cat`, not `Animal`.

```raven
let pet = if flag { Dog() } else { Cat() }
// pet has type: Dog | Cat
```

Functions and lambdas without an annotated return type infer their result by
collecting the types of all explicit `return` statements and the final expression
of the body. If no value-returning path exists, the type defaults to `unit`.

```raven
func example(x: int) -> {
    if x > 0 { return x }
    "neg"
}
// inferred return type: int | string
```

### Union conversions

Assigning or returning a union to an explicitly typed target succeeds only when
**every** member of the source union can convert to the target type. The compiler
checks each constituent individually.

```raven
let maybe = if flag { 0 } else { 1.0 } // int | float

let n: int = maybe    // error: float not assignable to int
let o: object = maybe // ok: both int and float convert to object

let pet = if flag { Dog() } else { Cat() } // Dog | Cat
let a: Animal = pet   // ok: Dog and Cat derive from Animal
let s: string = pet   // error: neither member converts to string
```

### String literals

```raven
let hello = "Hello, "
Console.WriteLine(hello + "World!")
Console.WriteLine("Hello, " + 2)
```

### String interpolation

Embed expressions directly into strings using `${...}` without requiring a prefix.

```raven
let name = "Alice"
let age = 30
let msg = "Name: ${name}, Age: ${age}"
Console.WriteLine(msg)
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

The `()` call operator invokes a function-valued expression. If the target
expression's type defines an invocation operator via a `self` method, that
member is invoked instead; see [Invocation operator](#invocation-operator).

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
(or `()` if none).

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

### `for` expression

Iterates over each element of a collection, binding it to a fresh local. The optional
`each` keyword improves readability.

```raven
for each item in items {
    Console.WriteLine(item)
}
```

The `each` keyword may be omitted:

```raven
for item in items {
    doSomething(item)
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

Import directives appear at the beginning of a compilation unit or namespace. All
imports for a given scope must come before any alias directives or member
declarations. Placing an import directive after an alias or member is a
compile-time error (`RAV1005`).

### Alias directive

The `alias` directive assigns an alternative name to a fully qualified
**namespace**, type, static member, or to any type expression such as tuples
and type unions.

```raven
alias IO = System.IO
alias SB = System.Text.StringBuilder
alias PrintLine = System.Console.WriteLine
alias Pair = (x: int, y: int)
alias Number = int | string
alias Flag = bool
alias Text = string

let sb = SB()
PrintLine("Hi")
let tmp = IO.Path.GetTempPath()
```

Aliasing a method binds a specific overload. Multiple directives using the
same alias name may appear to alias additional overloads, forming an overload
set.

Predefined types may be aliased directly. The supported built-in alias targets are `bool`, `char`, `int`, `string`, and `unit` (spelled `unit` or `()`).
Raven has no `void`; `unit` is projected to and from .NET `void`.
If the alias target is invalid, the compiler emits diagnostic `RAV2020`, which lists the supported targets such as types, namespaces, unions, tuples, and these predefined types.

Aliases require fully qualified names for namespaces, types, and members to
avoid ambiguity; type expressions are written directly. Alias directives may
appear at the top of a file or inside a namespace but must follow all import
directives and precede any declarations or statements. An alias directive that
appears after a member declaration produces diagnostic `RAV1006`.

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

### File-scope code rules

Files may start with executable statements that aren't enclosed in a function or type. This file-scope code forms the application's entry point and is translated into `Program.Main`. Only console applications may include file-scope code, and it may appear in at most one file per compilation. When present, these statements must come before any other declarations in the file or its file-scoped namespace.

Function declarations (local function statements) within file-scope code are hoisted and may be referenced from anywhere in that file-scoped region, regardless of their order.

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

### Nested functions

Functions may be declared inside other functions. Such a function is
scoped to its containing body and can capture local variables.

```raven
func outer() {
    func inner(x: int) -> int { x + 1 }
    let y = inner(2)
}
```

### `ref`/`out` arguments

Parameters can be declared by reference using `&Type`. Use `out` before
the parameter name to indicate that the value must be assigned by the
callee. At call sites, pass the argument with the address operator `&`.
(Exact rules are contextual; the binder enforces that the target is
assignable.)

```raven
func TryParse(text: string, out result: &int) -> bool { /* ... */ }

var total = 0
if !TryParse(arg, &total) {
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

### Tuple types

Tuple types use parentheses with comma-separated element types and map to
`System.ValueTuple`:

```raven
let pair: (int, string) = (42, "answer")
```

Elements may optionally be named with a `name: Type` pair. Names exist only for
developer clarity and do not participate in type identity or assignment:

```raven
let tuple2: (id: int, name: string) = (no: 42, identifier: "Bar")
```

When a tuple expression is assigned to an explicitly annotated tuple type, each
element is validated against the corresponding element type. Elements are
accessed positionally (e.g. `Item1`, `Item2`). Tuple types may nest or
participate in other type constructs such as unions or nullability.

### Union types

Unions express multiple possible types (e.g., `int | string`).

Union members are normalized: nested unions flatten, duplicates are removed,
and order is irrelevant. `int | (string | int)` therefore simplifies to
`int | string`.

The special `null` type may appear as a union member, usually via control
flow:

```raven
let maybe = if flag { 1 } else { null }
// maybe is inferred as: int | null
```

If a union contains `null` and exactly one non-nullable type, it implicitly
converts to that type's nullable form (`int | null` converts to `int?`).
Conversely, explicitly including a nullable type in a union—`string? | int`
—is a compile-time error.

Explicit annotations follow the same rules:

```raven
func test(x: int | string) -> () { /* ... */ }
```

Unions also arise naturally from control flow:

```raven
let x = 3
let y = if x > 2 { 40 + w; } else { true; }
// y is inferred as: int | bool
```

See [Union conversions](#union-conversions) for how union values interact with
explicit target types and return annotations.

If a literal value does not match the annotated union, diagnostics show the
literal itself in keyword form. For example:

```raven
let x: "true" | 1 = true
// error: Cannot assign 'true' to '"true" | 1'
```

Boolean literals appear lowercase (`true`/`false`) to align with their
keywords, and string or character literals retain their quotes.

Discrimination via `is`:

```raven
if y is int a {
    Console.WriteLine(a)
}
else if y is bool b {
    Console.WriteLine(b)
}
```

#### Metadata projection

When emitted to .NET metadata, a union is projected as the narrowest common
denominator of its members. If every member shares a base class, that base type
becomes the metadata type; otherwise, `object` is used. Including `null` in the
union marks the emitted type as nullable.

For example:

```raven
let pet = if flag { Dog() } else { Cat() } // Dog | Cat
```

Emits `Animal` because both `Dog` and `Cat` derive from it. In contrast:

```raven
let value = if flag { 0 } else { "hi" } // int | string | null
```

Emits `object?` since `int` and `string` share no base class other than
`object`, and `null` is included.

This narrowing makes unions friendlier to inheritance-based languages such as
C#, and it gives the runtime a smaller set of types to resolve. The
`TypeUnionsAnalyzer` provides additional hints about possible targets so that
consumers can work with the projected type more effectively.

To preserve the original union members, the compiler also attaches a
`TypeUnionAttribute` to the parameter or return type in metadata. The attribute
lists the CLR `Type` for each member in the union. The method signature itself
uses the narrowed base type (or `object`) as described above.

Raven emits shim types so that every union member has a concrete `Type`:

* `Unit` represents the Raven `unit` value and is emitted into every assembly.
* `Null` represents the `null` literal and is emitted only when a union includes
  `null`.

When `null` participates, the signature type is additionally marked as nullable.

For example:

```raven
func f(x: string | unit | null) -> unit { }
```

Emits a parameter of type `object?` with:

```csharp
[TypeUnionAttribute(typeof(string), typeof(Unit), typeof(Null))]
```

attached, indicating the full set of possible values.

### Nullable types

Appending `?` to a type denotes that it may also be `null`. This works for
both reference and value types.

```raven
let s: string? = null
let i: int? = null
```

Nullable types participate in the type system and overload resolution.

### Enums

An enum declaration introduces a distinct type whose instances are one of a
fixed set of named constants. Each member is implicitly static and has an
underlying integer value starting at `0` and increasing by one. Explicit numeric
values are not yet supported.

```raven
enum Grades { A, B, C }
```

Enum members can be referenced with the type name or, when the target type is
known, with a **leading dot**:

```raven
var grade: Grades = .B
grade = Grades.C
```

Importing the members of an enum brings them into scope:

```raven
import Grades.*
let best = A
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

    public Increment() -> () => _value = _value + 1
}
```

**Notes**

* Fields use `let`/`var` and require `;` after declarators.
* Accessor-level access (e.g., `private set`) is supported.
* Methods/ctors/properties/indexers may use arrow bodies.
* Members can be marked `static` to associate them with the type rather than an instance.

### Class inheritance

Classes are sealed by default. Marking a class `open` allows it to be used as a base type. A base class is specified with a colon
followed by the type name:

```raven
open class Parent {}
class Child : Parent {}
```

If a derived class omits a constructor, the base class' parameterless constructor is invoked automatically. Access modifiers
(`public`, `internal`, `protected`, `private`) apply as usual; `protected` members are accessible to derived classes.

> **Limitations:** Only single inheritance is supported. Derived classes may currently chain only to parameterless base
> constructors. Interfaces and abstract classes are planned for future versions.

### Method overloading

Functions and methods may share a name as long as their parameter counts or
types differ. Overload resolution selects the best match based on argument
types, `out`/by-ref modifiers, and nullability. Ambiguous calls produce a
diagnostic.

```raven
class Printer
{
    public Print(x: int) -> () => Console.WriteLine(x)
    public Print(x: string) -> () => Console.WriteLine(x)
}

Print(42)
Print("hi")
```

### Invocation operator

Declaring a method named `self` makes instances of the type invocable with the
call operator `()`.

```raven
class Adder
{
    public self(x: int, y: int) -> int => x + y
}

let add = Adder()
let sum = add(1, 2) // calls self(1, 2)
```

Invocation operators can themselves be overloaded by providing multiple `self`
methods with different parameter signatures.

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