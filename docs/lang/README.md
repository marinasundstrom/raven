## Raven programming language

### Sub sections

* [Language specification](spec/language-specification.md)
* [Proposals](proposals)

### Vision

These examples show the envisioned syntax of the language:

```csharp
import System

Console.Write("What is your name? ")

let name = Console.ReadLine()

if name is not null {
    Console.Write($"Hello, {name}!")
}
```