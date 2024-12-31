# Raven Programming Language

**Raven** is an experimental compiler inspired by the .NET Roslyn compiler architecture.

This project is built for fun and learning, with the purpose of designing a modern compiler that provides an API for manipulating syntax in an efficient, immutable manner—a concept often referred to as "Compiler-as-a-Service."

Raven is a merger and continued work on the projects from the [compiler-projects](https://github.com/marinasundstrom/compiler-projects) repository.

---

## Why the Name "Raven"?

The name reflects the character and essence of the language.

Ravens are remarkable birds, known for their intelligence and adaptability. In Old Norse mythology, ravens held significant importance as messengers of Odin, the chief god. His two ravens, **Huginn** ("thought") and **Muninn** ("memory" or "mind"), symbolized the power of intellect and reflection—qualities that align with the goals of this programming language.

The choice of "Raven" as the name is also a nod to its mythological roots and the fascinating traits of these birds. Alternatively, we could consider using the Old Norse term **"Hrafn"** or the Danish **"Ravn"** to further emphasize these connections.

---

## Project Goals

- **Create a Programming Language**: Build a programming language from the ground up, covering essential aspects of language design and implementation.
- **Focus on Parsing and Semantic Analysis**: Dive into the foundational processes of parsing and semantic analysis to understand and implement key components of a compiler.
- **Serve as a Reference for Compiler Construction**: Provide a documented reference for those interested in compiler construction, detailing the development process and design decisions.
- **Pragmatic Scope**: Acknowledge the extensive features of mature frameworks like Roslyn (over 10 years in development) and focus on implementing a feasible subset of features rather than aiming for full parity.

---

## Syntax

See the pseudo-specification [here](/docs/lang/spec/language-specification.md).

### Sample

Here’s a sample of the Raven syntax, taken from [this file](src/Raven.Compiler/test.rav):

```raven
import System;

Console.WriteLine("Input: ");

let str = Console.ReadLine();
let value = bool.Parse(str);

if (value) {
    Console.WriteLine("Hello" + ", World!");
} else {
    Console.WriteLine("boo");
}
```

---

## API

The compiler API is detailed [here](docs/compiler/api.md).

The [Raven.Compiler project](src/Raven.Compiler/Program.cs) demonstrates how to utilize the API in practice.

---

## Development

This project is being developed through a combination of:

- **AI Assistance**: Using AI for guidance on architecture and generating initial code.
- **Source Code Analysis**: Studying the source code of the C# compiler for insights.
- **Reverse Engineering**: Examining decompiled sources of the C# compiler to understand its inner workings.

For a deeper look into the language's structure, check out the [unit tests](/Raven.CodeAnalysis.Tests/Syntax/AstTest.cs).

---

## Documentation

Comprehensive project documentation is available [here](/docs/).
