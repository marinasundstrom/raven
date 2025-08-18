# Proposal: Function declarations

> ℹ️ This proposal has been partly implemented


This is how function and method declarations are envisioned:

```csharp
Add(a : int, b : int) -> int {
    a + b;
}
```

### Question:

Perhaps we should introduce some kind of keyword, like `def`.

## Expression-bodied

```csharp
Add(a : int, b : int) -> int => a + b;
```

## Type inference

With type inference and generics:

```csharp
Add(a, b) => a + b;
```

Any type that supports the `+` operator will be valid as argument for parameter `a` and `b`.