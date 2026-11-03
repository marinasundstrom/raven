#if RAVEN_PORT
extern alias Port;
using TestPrinterOptions = Port::Raven.CodeAnalysis.Syntax.PrinterOptions;
using TestPrinter = Port::Raven.CodeAnalysis.Syntax.PrettySyntaxTreePrinter;
#else
using TestPrinterOptions = Raven.CodeAnalysis.Syntax.PrinterOptions;
using TestPrinter = Raven.CodeAnalysis.Syntax.PrettySyntaxTreePrinter;
#endif
using System.IO;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class PrettySyntaxTreePrinterTests
{
#if RAVEN_PORT
    [Fact]
    public void ComponentTestsSelectTheRavenPrinter()
    {
        Assert.Equal("Raven.CompilerParity.CSharp.Tests", typeof(TestPrinter).Assembly.GetName().Name);
        Assert.Same(typeof(TestPrinter).Assembly, typeof(TestPrinterOptions).Assembly);
    }
#endif

    [Fact]
    public void PrintSyntaxTree_ToTextWriter_ProducesExpectedOutput()
    {
        var (printed, representation) = PrintTree("let x = 1;", new TestPrinterOptions
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
        var (printed, representation) = PrintTree("let x = 1;", new TestPrinterOptions
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

    [Theory]
    [InlineData(0, false, false)]
    [InlineData(1, true, false)]
    [InlineData(2, true, true)]
    public void MaxDepthCountsFromTheRoot(int maxDepth, bool includesOperands, bool includesOperandTokens)
    {
        var root = SyntaxFactory.ParseExpression("left + right");
        var options = new TestPrinterOptions { Colorize = false, MaxDepth = maxDepth };
        var representation = TestPrinter.GetSyntaxTreeRepresentation(root, options);
        using var writer = new StringWriter();
        TestPrinter.PrintSyntaxTree(root, options, writer);
        Assert.Equal(representation, writer.ToString());
        Assert.Contains("AddExpression", representation);
        Assert.Equal(includesOperands, representation.Contains("IdentifierName"));
        Assert.Equal(includesOperandTokens, representation.Contains("IdentifierToken"));
    }

    [Theory]
    [InlineData(1, false)]
    [InlineData(2, true)]
    public void MaxDepthAlsoLimitsTokenTrivia(int maxDepth, bool includesTrivia)
    {
        var root = SyntaxFactory.ParseExpression(" left ");
        var options = new TestPrinterOptions { Colorize = false, IncludeTrivia = true, MaxDepth = maxDepth };
        var printed = TestPrinter.GetSyntaxTreeRepresentation(root, options);
        Assert.Contains("IdentifierToken", printed);
        Assert.Equal(includesTrivia, printed.Contains("WhitespaceTrivia"));
    }

    private static (string Printed, string Representation) PrintTree(string code, TestPrinterOptions options)
    {
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();

        using var writer = new StringWriter();
        TestPrinter.PrintSyntaxTree(root, options, writer);
        var printed = writer.ToString();

        var representation = TestPrinter.GetSyntaxTreeRepresentation(root, options);
        return (printed, representation);
    }
}
