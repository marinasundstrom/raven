# Proposal: Indentation

> ❌ This proposal has been rejected

This is an important decision to make:

Should we use indentation for blocks instead of curly braces?

Would distinguish the language from other languages that are C-style, in particular C#.

## Alternatives

### 1. Curly braces

C-like style that has been implemented to this date:

```csharp
import System.*;

Console.Write("What is your name? ");

let name = Console.ReadLine();

if name is not null {
    Console.Write($"Hello, {name}!");
} else
    return 2;
```

### 2. Indentation

Using Python-like indentation (and no mandatory semicolons):

```csharp
import System.*

Console.Write("What is your name? ")

let name = Console.ReadLine()

if name is not null:
    Console.Write($"Hello, {name}!")
else
    return 2
```

We need to make this better than in Python. 

Newlines will be end of statements unless in a construct like an expression.

## Notes

**Alternative 2**: Semicolons and curly braces could theoretically be optional.

In the case of a one-liner:

```csharp
let name = Console.ReadLine(); Console.Write($"Hello, {name}!")
```