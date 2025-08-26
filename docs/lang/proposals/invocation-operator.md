# Proposal: Invocation operator

> ℹ️ This feature has been implemented

This document outlines how to define an invocation operator which makes an object behave like a callable function object.

You can define one or more unique invocation operators for your type.

## Syntax

The syntax for declaring an invocation operator is this. (as defined in a class)

```raven
public class Test {
    public self(no: int) -> string {
        return "Foo"
    }
}
```

When "invoking" an instance of a type with an invocation operator:

```raven
let test = Test()
let x = test(2)
```

### Implementation details

The compiler produces something that can be represented this way in C#:

```csharp
[Invokable]
public class Test 
{
    public string Invoke(int no) 
    {
        return "Foo";
    }
}
```

With the invocation as such:

```csharp
var test = new Test();
var x = test.Invoke(2);
```

## Notes

The `Invoke` method mirrors the one of delegates.

