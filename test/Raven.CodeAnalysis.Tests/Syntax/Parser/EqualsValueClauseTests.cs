using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class EqualsValueClauseTests
{
    [Fact]
    public void VariableDeclaration_MissingInitializer_ProducesDiagnostic()
    {
        var code = "let x =;";
        var tree = SyntaxTree.ParseText(code);
        var diagnostic = Assert.Single(tree.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.ExpressionExpected, diagnostic.Descriptor);
    }
}
