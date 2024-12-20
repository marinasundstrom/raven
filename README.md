# Raven programming language

Experimental compiler based on the .NET Roslyn compiler architecture.

Built for fun and learning!

The purpose is to build a modern compiler that provides an API for manipulating syntax is an efficient immutable fashion. This could be referred to as a "Compiler-as-a-Service".

It is a merger of the projects in the [compiler-projects](https://github.com/marinasundstrom/compiler-projects) repo.

Look at [unit tests](/Raven.Tests/CodeAnalysis/Syntax/AstTest.cs).

## Why the name "Raven"?

The name decides the character of the language.

Raven is a species of birds, and ravens are cool. They are important in Old Norse mythology because chief god Odin had two ravens, Huginn ("thought") and Muninn ("memory, mind"), who were his messengers.

Perhaps we should use the Old Norse form "hrafn", or Danish "ravn"?

## Syntax

See pseudo-specification [here](/docs/lang/spec/language-specification.md).

### Sample

From this [file](Raven.Compiler/test.rav):

```
import System;
import System.Net;

let x : int = 2

if (x > 2 ) {
    return (6 + 2) * 2;;
} else
    return foo.bar(2)
        .GetId(1, z + 2, "Foo")
```

## API

The compiler API is described [here](docs/compiler/api.md).

The actual compiler app [project](Raven.Compiler) demonstrates how to utilize the API.

## Development

This project is being developed using the following means:
* Using AI for answers, about the architecture, and for code to get started.
* Looking at the actual source code for C# compiler.
* Looking at de-compiled sources of C# compiler.

So it's quite a lot of reverse engineering.

## Documentation

Read the documentation [here](/docs/).