# Proposal: Unit type

> ℹ️ This feature has been implemented

The `unit` type represents a value in absence of a value. It can be seen as `void` but the difference is that it is a real type with exactly one value. Once the feature is reintroduced, `unit` will wholly replace the language's `void` concept so that every callable returns a concrete type.

The keyword `unit` maps to the type `System.Unit` (see definition below).

## Purpose

- Replace `void` with a first-class `unit` value so all functions have a real return type.
- Enable type flow where `void` cannot be used (generics, tuples, unions).
- Provide a concrete value for "empty", distinct from `null` or `void`.
 
## Rationale

Raven treats the absence of a result as a first-class value so that every expression has a type. A dedicated `unit` type avoids special cases around `void`, letting side-effect-only functions compose with generics, tuples, and unions. This mirrors functional languages where even "do nothing" computations produce a value and encourages explicit handling of effects.

## Syntax

```raven
let v = Foo()   // v : unit

func Foo() -> unit {
    () // return unit
}
```

* `unit` is a valid type specifier.
* Functions without explicit return type default to `unit`.
* The literal `()` refers to the single value of this type.

## Semantics

* `unit` is a builtin type with one value.
* Control-flow and block expressions return `unit` when no other type is inferred.
* `unit` may appear in unions and tuples.
* Declaring parameters of type `unit` is discouraged and may be disallowed; fields and properties cannot be solely `unit`, though they may use unions that include it.
* The `unit` type typically arises from control flow and type inference rather than explicit user annotations.
* The `()` literal may only appear as an explicit `return ()` or as the implicit result of a block; standalone expressions such as `[()]` or `(1, ())` are invalid.
* Because `unit` is a concrete value, `return` statements must supply an expression; `return;` is invalid. Use `return ()` for an explicit unit or fall through to return it implicitly.
* `unit` participates in pattern matching like any other value. A dedicated `match` expression is a separate proposal; existing `if`-expression pattern syntax (`if x is () { ... }`) already works.

```raven
func Foo(ok: bool) -> int | unit {
    if ok {
        return 42
    }
    return ()
}
```

> In the future we might enforce the handling of `unit`, either by using the returned value or by explicitly discarding it (e.g. `_ = Console.WriteLine("")`). This enforces functional programming patterns.

### Control flow

> **Note:** Type unions are not explained in this proposal. Just remember that `unit` is a type like any other.

```raven
let x = if true { 42 } // x : int | unit
```

```raven
let x = if true { 42 } else { Console.WriteLine("Test2") } //  x : int | unit
```

### Aliasing

Just like other built in types, `unit` can be aliased.

```raven
alias MyUnit = unit
```

### Additional examples

#### Generics and tuples

A `unit` value must originate from control flow; bare literals like `[()]` or `(1, ())` are invalid.

```raven
func ping() { }

let u = ping()
let list: List<unit> = [u]
let pair: (int, unit) = (1, u)
```

C#:

```csharp
void Ping() { }

Ping();
var u = Unit.Value;
var list = new List<Unit> { u };
var pair = (1, u);
```

#### Pattern matching

Pattern matching with a `match` expression will be introduced in a separate proposal. Until then, `unit` works with the existing pattern syntax in `if` expressions.

```raven
let u = ping()
if u is () {
    Console.WriteLine("ok")
}
```

C#:

```csharp
Ping();
var u = Unit.Value;
if (u == Unit.Value)
{
    Console.WriteLine("ok");
}
```

> **Note:** We will be reworking the the `if` pattern syntax to fit the style of language. Suggesting inspiration from Swift, F#, Kotlin, and Rust.

#### Explicit discarding

```raven
_ = Console.WriteLine("logged")
```

C#:

```csharp
_ = Console.WriteLine("logged");
```

#### Implicit `unit` return

```raven
func Outer() {
    func Inner() { }
}
```

C#:

```csharp
void Outer() {
    void Inner() { }
}
```

### Implementation details

* `UnitTypeSymbol` is its own symbol.
* Functions and local functions without an explicit return type are bound to return `unit`.
* `unit` is a keyword token for the type; the literal value is spelled `()`.
* The parser treats a standalone `()` in expression position as a `UnitLiteralExpression`; parenthesized expressions must contain an inner expression, and `()` following an expression denotes an empty argument list.
* Control-flow statements without an explicit value produce `unit`.
* Methods returning `unit` emit IL `ret` without pushing a value; callers that require a `unit` result must load `Unit.Value` after the call.

### Reimplementation considerations

When reintroducing the feature:

* Reserve the `unit` keyword in the lexer and parser.
* Ensure binder defaults missing return types to `unit` for top-level and local functions.
* Introduce a built-in `UnitTypeSymbol` and a `UnitLiteralExpression` syntax node.
* Contextually parse `()` as the unit literal when it appears on its own rather than as an argument list or grouped expression.
* Remove the `return` statement form without an expression.
* Map `unit` to a `System.Unit` struct during lowering and code generation.
* Avoid emitting the `Unit.Value` field unless the value is consumed.
* Add tests covering generics, tuples, interop with `void`, and default return types.
* Use `()` as the unit literal, matching functional-language conventions and the `Unit.ToString()` output.

## Interop

* **From Raven**: Methods that return `unit` emit as `void` by default, unless a real type is required (e.g. generics).
* **To Raven**: External `void` return types are projected as `unit`.

## Code generation

> **Note**: We are mainly showing the representation of constructs in C#. Although the emitted form is in metadata and IL.

### Methods return `void` instead of `unit`

Methods declared with a `unit` return type are emitted as `void`. If a call site needs the resulting value (for example, to pass
into a generic method or to store in a variable), the compiler inserts a load of `Unit.Value` after the call.

```raven
class Test {
    Foo() -> unit {

    }

    Foo2() { // implicit -> Unit

    }
}
```

C#:

```csharp
class Test
{
    void Foo() { }
    void Foo2() { }
}
```

### Emit `unit` only when used

Codegen will not emit any `unit` value for constructs, such as expressions and statements in block, unless the surrounding code consumes the result.

#### Method declarations

Since at a metadata-level, `void` is used instead of `unit`:


```raven
class Test {
    Foo() {
        Console.WriteLine("Test")
    }
}
```

The equivalent in C# would be:

```csharp
class Test 
{
    void Foo() 
    {
        Console.WriteLine("Test")
    }
}
```

#### Using `unit` in expressions

Other example:

```raven
let x = Console.WriteLine("Test") // x: unit = ()
Console.WriteLine(Console.WriteLine("Test")) // prints "()"
```

The representation in C#:

```csharp
Console.WriteLine("Test");
var x = Unit.Value;

Console.WriteLine("Test");
Console.WriteLine(Unit.Value);
```

#### Using `unit` in control flow statements

Here is control flow in an if statement.

```raven
var check = true
let x = if check { Console.WriteLine("Test1") } else { 42 } // x: unit | int
```

The representation in C#:

```csharp
bool check = true;
object x = default; // Semantically: Union between unit and 42
if(check)
{
    x = Unit.Value;
}
else
{
    x = 42;
}
```

> **Point is:** We should avoid emitting unit unless used in code.

## `Unit` struct definition

```csharp
public readonly struct Unit : IEquatable<Unit>
{
    public static readonly Unit Value = default;
    public bool Equals(Unit other) => true;
    public override bool Equals(object? obj) => obj is Unit;
    public override int GetHashCode() => 0;
    public override string ToString() => "()";
}
```

The literal `()` maps to `Unit.Value`.

> Initially this type will be embedded in Raven assemblies, later moved to a shared core library.

## Snippets

> To be used by the code gen.

### Generate the `Unit` struct

```csharp
private void CreateUnitStruct()
{
    var unitBuilder = ModuleBuilder.DefineType(
        "Unit",
        TypeAttributes.Public | TypeAttributes.Sealed | TypeAttributes.SequentialLayout,
        Compilation.GetTypeByMetadataName("System.ValueType").GetClrType(this));

    var valueField = unitBuilder.DefineField(
        "Value",
        unitBuilder,
        FieldAttributes.Public | FieldAttributes.Static | FieldAttributes.InitOnly);

    var equalsMethod = unitBuilder.DefineMethod(
        "Equals",
        MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.Virtual | MethodAttributes.Final,
        Compilation.GetTypeByMetadataName("System.Boolean").GetClrType(this),
        new[] { unitBuilder });
    var ilEquals = equalsMethod.GetILGenerator();
    ilEquals.Emit(OpCodes.Ldc_I4_1);
    ilEquals.Emit(OpCodes.Ret);

    var equalsObjMethod = unitBuilder.DefineMethod(
        "Equals",
        MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.Virtual | MethodAttributes.Final,
        Compilation.GetTypeByMetadataName("System.Boolean").GetClrType(this),
        new[] { Compilation.GetTypeByMetadataName("System.Object").GetClrType(this) });
    var ilEqualsObj = equalsObjMethod.GetILGenerator();
    ilEqualsObj.Emit(OpCodes.Ldarg_1);
    ilEqualsObj.Emit(OpCodes.Isinst, unitBuilder);
    ilEqualsObj.Emit(OpCodes.Ldnull);
    ilEqualsObj.Emit(OpCodes.Cgt_Un);
    ilEqualsObj.Emit(OpCodes.Ret);

    var getHashCodeMethod = unitBuilder.DefineMethod(
        "GetHashCode",
        MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.Virtual,
        Compilation.GetTypeByMetadataName("System.Int32").GetClrType(this),
        Type.EmptyTypes);
    var ilHash = getHashCodeMethod.GetILGenerator();
    ilHash.Emit(OpCodes.Ldc_I4_0);
    ilHash.Emit(OpCodes.Ret);

    var toStringMethod = unitBuilder.DefineMethod(
        "ToString",
        MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.Virtual,
        Compilation.GetTypeByMetadataName("System.String").GetClrType(this),
        Type.EmptyTypes);
    var ilToString = toStringMethod.GetILGenerator();
    ilToString.Emit(OpCodes.Ldstr, "()");
    ilToString.Emit(OpCodes.Ret);

    UnitType = unitBuilder.CreateType();
}
```

### Emitting `Unit` value

```csharp
private void EmitUnitExpression(BoundUnitExpression unitExpression)
{
    var unitType = MethodGenerator.TypeGenerator.CodeGen.UnitType
        ?? throw new InvalidOperationException("Unit type was not emitted.");
    var valueField = unitType.GetField("Value")
        ?? throw new InvalidOperationException("Unit.Value field missing.");
    ILGenerator.Emit(OpCodes.Ldsfld, valueField);
}
```

### Notes

* If there are any inconsistencies in language rules, please refer to how it is done in other functional style programming languages, primarily F#. Weigh the pros and cons and whether it fits into Raven.