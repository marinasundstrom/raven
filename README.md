# Raven programming language

Experimental compiler based on the .NET Roslyn compiler architecture.

Built for fun and learning!

The purpose is to build a modern compiler that provides an API for manipulating syntax is an efficient immutable fashion. This could be referred to as a "Compiler-as-a-Service".

It is a merger of the projects in the [compiler-projects](https://github.com/marinasundstrom/compiler-projects) repo.

Look at [unit tests](/Raven.Tests/CodeAnalysis/Syntax/AstTest.cs).

## Syntax

See pseudo-specification [here](/docs/syntax.md).

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
                Parameter(),
                CommaToken,
                Parameter()
            ])
        ))
    .WithBody(
        Block(
            List<StatementSyntax>(
                ifStatementWithElseClause)));
```



