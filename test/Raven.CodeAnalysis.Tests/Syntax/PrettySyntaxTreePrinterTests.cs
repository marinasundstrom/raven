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

        Assert.Equal(representation, printed);
        Assert.Contains("CompilationUnit", printed);
        Assert.Contains("Statement: LocalDeclarationStatement", printed);
        Assert.Contains("BindingKeyword: let LetKeyword", printed);
        Assert.Contains("EndOfFileToken:  EndOfFileToken", printed);
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

        Assert.Equal(representation, printed);
        Assert.Contains("CompilationUnit", printed);
        Assert.Contains("Statement: LocalDeclarationStatement", printed);
        Assert.Contains("VariableDeclarator", printed);
        Assert.Contains("EndOfFileToken:  EndOfFileToken", printed);
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
