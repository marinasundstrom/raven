using System.IO;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class PrettySyntaxTreePrinterTests
{
    [Fact]
    public void PrintSyntaxTree_ToTextWriter_ProducesExpectedOutput()
    {
        var (printed, representation) = PrintTree("let x = 1;", new PrinterOptions
        {
            IncludeTokens = true,
            IncludeTrivia = false,
            IncludeSpans = false,
            IncludeLocations = false,
            IncludeNames = true,
            Colorize = false
        });

        const string expected =
            """
            CompilationUnit
            ├── Members
            │   └── GlobalStatement
            │       └── Statement: LocalDeclarationStatement
            │           ├── Declaration: VariableDeclaration
            │           │   ├── BindingKeyword: let LetKeyword
            │           │   └── Declarators
            │           │       └── VariableDeclarator
            │           │           ├── Identifier: x IdentifierToken
            │           │           └── Initializer: EqualsValueClause
            │           │               ├── EqualsToken: = EqualsToken
            │           │               └── Value: NumericLiteralExpression
            │           │                   └── Token: 1 NumericLiteralToken
            │           └── TerminatorToken: ; SemicolonToken
            └── EndOfFileToken:  EndOfFileToken
            """ + "\n";

        Assert.Equal(representation, printed);
        Assert.Equal(expected, printed);
    }

    [Fact]
    public void PrintSyntaxTree_ToTextWriter_WithExpandedLists_ProducesPreviousStructure()
    {
        var (printed, representation) = PrintTree("let x = 1;", new PrinterOptions
        {
            IncludeTokens = true,
            IncludeTrivia = false,
            IncludeSpans = false,
            IncludeLocations = false,
            IncludeNames = true,
            Colorize = false,
            ExpandListsAsProperties = true
        });

        const string expected =
            """
            CompilationUnit
            ├── GlobalStatement
            │   └── Statement: LocalDeclarationStatement
            │       ├── Declaration: VariableDeclaration
            │       │   ├── BindingKeyword: let LetKeyword
            │       │   └── VariableDeclarator
            │       │       ├── Identifier: x IdentifierToken
            │       │       └── Initializer: EqualsValueClause
            │       │           ├── EqualsToken: = EqualsToken
            │       │           └── Value: NumericLiteralExpression
            │       │               └── Token: 1 NumericLiteralToken
            │       └── TerminatorToken: ; SemicolonToken
            └── EndOfFileToken:  EndOfFileToken
            """ + "\n";

        Assert.Equal(representation, printed);
        Assert.Equal(expected, printed);
    }

    private static (string Printed, string Representation) PrintTree(string code, PrinterOptions options)
    {
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();

        using var writer = new StringWriter();
        root.PrintSyntaxTree(options, writer);
        var printed = writer.ToString();

        var representation = root.GetSyntaxTreeRepresentation(options);
        return (printed, representation);
    }
}
