# Proposal: Member binding expressions

> ✅ Implemented via a dedicated `MemberBindingExpression` syntax node.

## Summary

Introduce `MemberBindingExpression` to allow target-typed member access using a leading `.`. The target type of the assignment or context determines the member to bind to (assignment, return position, arguments, and pattern contexts).

## Syntax

```
.MemberName
.MemberName(argument1, argument2)
```

Grammar:

```
member-binding-expression := '.' identifier argument-list?
```

## Examples

### Enum value

```raven
enum Color {
    Red,
    Green,
    Blue,
}

let favorite: Color = .Red
```

### Static property

```raven
let x: string = .Empty
```

### Static method

```raven
let number: int = .Parse("42")
```

The target type on the left-hand side guides resolution of the member.

## Target-typed contexts

Member binding expressions rely on a target type supplied by context. The binding is permitted when a target type can be inferred from:

- Variable declarations or assignment expressions.
- Return statements (including implicit returns).
- Argument positions for method **and constructor** invocations.
- Pattern positions (e.g., match arms) where a target type is available.

## Motivation

Member binding expressions reduce verbosity when referring to enum members or static members of a known type. They enable concise, target-typed initialization and invocation.

## Open questions

- Should member binding be allowed in expression contexts other than assignment?
- How does overload resolution interact with member binding?
