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

See pseudo-specification [here](/docs/syntax.md).

## Development

This project is being developed using the following means:
* Using AI for answers, about the architecture, and for code to get started.
* Looking at the actual source code for C# compiler.
* Looking at de-compiled sources of C# compiler.

So it's quite a lot of reverse engineering.

## Documentation

Read the documentation [here](/docs/).

### Syntax Tree

Here's an example of the AST in C#. As taken from the unit tests.

```csharp
using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

var ifStatement = IfStatement(
    condition: BinaryExpression(
        IdentifierName("x"),
        GreaterThanToken,
        IdentifierName("y")),
    statement: Block(List<StatementSyntax>(
        ReturnStatement(
            LiteralExpression(2))
    )));

var ifStatementWithElseClause = ifStatement
        .WithElseClause(
            ElseClause(
                ReturnStatement(
                    LiteralExpression(2))));

var methodDeclaration = MethodDeclaration(
        ParseTypeName("test"),
        IdentifierName("FooBar"),
        TypeParameterList(
            SeparatedList<ParameterSyntax>([
                Parameter(IdentifierName("a")),
                CommaToken,
                Parameter(IdentifierName("b"))
            ])
        ))
    .WithBody(
        Block(
            List<StatementSyntax>(
                ifStatementWithElseClause)));
```



