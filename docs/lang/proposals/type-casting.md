# Proposal: Type casting

> ⚠️ This proposal has **NOT** been implemented

This document outlines explicit cast expressions and the `as` operator in Raven, following C# semantics.

## Purpose

Allow developers to perform conversions that are not covered by implicit rules, such as downcasting or numeric narrowing.

## Syntax

### Explicit cast

`(T)expr` forcefully converts `expr` to type `T`. If the runtime conversion fails, an exception is thrown.

```raven
let d = (double)1
let dog = (Dog)animal
```

### `as` operator

`expr as T` attempts the conversion and yields `null` (or a nullable value type) when it fails.

```raven
let dog = animal as Dog
if dog != null {
    // safe use of dog
}
```

## Examples

```raven
class Animal {}
class Dog : Animal { bark() -> () => Console.WriteLine("bark") }

let animal: Animal = Dog()
let dog1 = (Dog)animal      // succeeds
let dog2 = animal as Dog    // Dog?
let cat  = animal as Cat    // null
```

## Limitations

* Value types require nullable forms when used with `as`.
* Failed explicit casts raise an `InvalidCastException` at runtime.
* Pattern matching with `is` is handled separately.
