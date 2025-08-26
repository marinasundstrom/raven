using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class TupleTypeSyntaxTest
{
    [Fact]
    public void TupleType_WithPredefinedElements_Parses()
    {
        var code = "let x: (string, int) = (\"a\", 1)";
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var local = (LocalDeclarationStatementSyntax)((GlobalStatementSyntax)root.Members[0]).Statement!;
        var typeSyntax = local.Declaration.Declarators[0].TypeAnnotation!.Type;
        var tuple = Assert.IsType<TupleTypeSyntax>(typeSyntax);
        Assert.Equal(2, tuple.Types.Count);
        Assert.IsType<PredefinedTypeSyntax>(tuple.Types[0]);
        Assert.IsType<PredefinedTypeSyntax>(tuple.Types[1]);
    }
}
