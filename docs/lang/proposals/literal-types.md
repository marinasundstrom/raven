# Proposal: Literal-Value Types and Literal Union Types

> ✅ This proposal has been implemented

## Summary
Allow literal values and constants to appear directly in type positions and participate in union types. Each literal-value type represents exactly one value, enabling precise exhaustiveness checks and clearer APIs. Type unions already exist in Raven and semantically hold type elements; this proposal extends them so they may also include literal-value types.

## Syntax
```raven
let flag: true | false = true
let token: 0 | 1 | "admin" = 1

import System.Math
let angle: PI | () = Pi
```

A *literal-value type* is written with the literal itself. Literals may include numbers, strings, characters, booleans, or constant identifiers that resolve to compile‑time constants.

A *literal union* combines multiple literal-value types or mixes them with ordinary types using the existing `|` operator.

## Semantics
- Each literal-value type is a singleton subtype inhabited only by the literal itself.
- The compiler treats a union containing literals as a finite set of possible values.
- Type inference and pattern matching respect these singleton types.
- Existing type unions contain only type members; to include literals we treat each literal as its own type, using the literal's value when evaluating patterns.
- Literal expressions themselves are typed with their corresponding singleton type and implicitly convert to their underlying primitive type.

## Metadata Representation
Literal unions are emitted using `TypeUnionAttribute`. Each union member becomes a constructor argument. To support literal values, `TypeUnionAttribute` must accept `object` arguments rather than only `Type` instances:

```raven
func Foo(arg: (int | "yes" | 'c' | .2 | false)) {

}
```

In C#:

```csharp
public void Foo([TypeUnion(typeof(int), "yes", 'c', 0.2, false)] object arg) 
{
    
}
```

Constant identifiers are lowered to their literal values before emission. Consumers can reflect over `TypeUnionAttribute` to discover both type and literal members.

### Named constant values

> **Consider in future**

For named constants, we might also need to encode names of constants, and position of element.

```raven
import System.Math.*

func Foo() -> PI | () {
    
}
```

In C#:

```csharp
public enum TUPlaceholder { I0, I1, /* ... */ }

[AttributeUsage(AttributeTargets.ReturnValue, AllowMultiple = true)]
public sealed class TypeUnionAttribute : Attribute
{
    public TypeUnionAttribute(TUPlaceholder slot, Type type) { /* store */ }
}

[AttributeUsage(AttributeTargets.ReturnValue, AllowMultiple = true)]
public sealed class TypeUnionConstantAttribute : Attribute
{
    public TypeUnionConstantAttribute(
        TUPlaceholder slot,
        string source,          // e.g., "System.Math.PI" or "literal"
        Type type,              // the constant's type
        object value)           // boxed compile-time constant
    { /* store */ }
}

[return: TypeUnion(TUPlaceholder.I0, typeof(Unit))]
[return: TypeUnionConstant(TUPlaceholder.I0, "System.Math.PI", typeof(double), 3.1415926535897931)]
public object Foo()
{
    // ...
    return default!;
}
```

## Pattern Matching
Literal-value types participate in pattern matching. Any context that accepts a pattern—including the future `match` expression—may use literal patterns:

```raven
match token with
| 0 => "None"
| 1 => "One"
| "admin" => "Admin"
```

## Open Questions
- Should numeric literals respect implicit conversions when used as types?
- How are large or non-primitive constants encoded in metadata?

## Implementation details

>*This is just a concept*

The representation in the semantic model:

```csharp
sealed class LiteralTypeSymbol : ITypeSymbol
{
    public TypeSymbol UnderlyingType { get; } // Inherited
    public object ConstantValue { get; } // boxed, incl. enum constants

    // Equals/GetHashCode on (Underlying, ConstantValue)
    // SpecialType: None; IsReferenceType/IsValueType mirrors Underlying
}
```

We need conversions as well.