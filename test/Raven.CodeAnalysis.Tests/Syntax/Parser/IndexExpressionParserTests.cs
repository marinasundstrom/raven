using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class IndexExpressionParserTests
{
    [Fact]
    public void IndexExpression_WithoutWhitespace_Parses()
    {
        var tree = SyntaxTree.ParseText("val i = ^1");

        Assert.Empty(tree.GetDiagnostics());

        var index = tree.GetRoot().DescendantNodes().OfType<IndexExpressionSyntax>().Single();
        Assert.Equal("1", index.Expression.ToString());
    }

    [Fact]
    public void IndexExpression_WithWhitespaceAfterCaret_ReportsDiagnostic()
    {
        var tree = SyntaxTree.ParseText("val i = ^ 1");
        var diagnostics = tree.GetDiagnostics().ToArray();

        Assert.NotEmpty(diagnostics);
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.UnexpectedTokenInIncompleteSyntax);
    }
}
