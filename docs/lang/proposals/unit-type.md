# Proposal: Unit type

⚠️ This proposal has **NOT** been implemented

The `unit` type represents a value in absence of a value. It can be seen as `void` but the difference is that it is a real type with exactly one value.

The keyword `unit` maps to the type `System.Unit` (see definition below).

## Purpose

- Enable type flow where `void` cannot be used (generics, tuples, unions).
- Provide a concrete value for "empty", distinct from `null` or `void`.

## Syntax

```raven
let v = Foo()   // v : unit

func Foo() -> unit {
    unit // return unit
}
````

* `unit` is a valid type specifier.
* Functions without explicit return type default to `unit`.
* The literal `unit` refers to the single value of this type.

## Semantics

* `unit` is a builtin type with one value.
* Control-flow and block expressions return `unit` when no other type is inferred.
* `unit` may appear in unions and tuples.

```raven
func Foo(ok: bool) -> int | unit {
    if ok {
        return 42
    }
    return unit
}
```

> In the future we might enforce the handling of `unit`. Either b7 use or discard `_ = Console.WriteLine("")`. This enforces functional programming patterns.

### Control flow

> **Note:** Type unions is not explained in this proposal. Just remember that `unit` is a type like any other..

```csharp
let x = if true {.42 } // x : int | unit
```

```csharp
let x = if true {.42 } else { Console.WriteLine("Test2") } //  x : int | unit
```

### Aliasing

Just like other built in types, `unit` can be aliased.

```raven
alias MyUnit = unit
```

### Implementation details

* `UnitTypeSymbol` is its own symbol.

## Interop

* **From Raven**: Methods that return `unit` emit as `void` by default, unless a real type is required (e.g. generics).
* **To Raven**: External `void` return types are projected as `unit`.

## Code generation

> **Note**: We are mainly showing the representation of constructs in C#. Although the emitted form is in metadata and IL.

### Methods return `void` instead of `unit`

Method returning `unit` will have actual type `void`.

```raven
class Test {
    Foo() -> unit {

    }

    Foo2() { // implicit -> Unit

    }
}
```

C#

```raven
class Test 
{
    void Foo() { }
    void Foo2() { }
}
```

### Emit `unit` only when used

Codegen will not emit any `unit` value for constructs, such as expressions and statements in block, unless consumed by another construct.

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

```csharp
let x = Console.WriteLine("Test") // x: unit = unit

Console.WriteLine(Console.WriteLine("Test")) // Emit unit for inner WriteLine. Prints: "()" for unit
```

The representation in C#:

```csharp
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
    x = Unit.value;
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

The literal `unit` maps to `Unit.Value`.

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