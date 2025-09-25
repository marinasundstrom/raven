using System.IO;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class PrettySyntaxTreePrinterTests
{
    [Fact]
    public void PrintSyntaxTree_ToTextWriter_ProducesExpectedOutput()
    {
        var code = "let x = 1;";
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var options = new PrinterOptions
        {
            IncludeTokens = true,
            IncludeTrivia = false,
            IncludeSpans = false,
            IncludeLocations = false,
            IncludeNames = true,
            Colorize = false
        };

        using var writer = new StringWriter();
        root.PrintSyntaxTree(options, writer);
        var printed = writer.ToString();

        var representation = root.GetSyntaxTreeRepresentation(options);

        const string expected =
            """
            CompilationUnit
            ├── GlobalStatement
            │   └── Statement: LocalDeclarationStatement
            │       ├── Declaration: VariableDeclaration
            │       │   ├── LetOrVarKeyword: let LetKeyword
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
}
