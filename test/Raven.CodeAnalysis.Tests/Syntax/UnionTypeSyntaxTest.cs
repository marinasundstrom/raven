using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class UnionTypeSyntaxTest
{
    [Fact]
    public void UnionType_WithNullElement_UsesNullTypeSyntax()
    {
        var code = "let x: string | null = null";
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var local = (LocalDeclarationStatementSyntax)((GlobalStatementSyntax)root.Members[0]).Statement!;
        var typeSyntax = local.Declaration.Declarators[0].TypeAnnotation!.Type;
        var union = Assert.IsType<UnionTypeSyntax>(typeSyntax);
        Assert.IsType<PredefinedTypeSyntax>(union.Types[0]);
        Assert.IsType<NullTypeSyntax>(union.Types[1]);
    }

    [Fact]
    public void UnionType_WithLiteralElements_UsesLiteralTypeSyntax()
    {
        var code = "let flag: true | false = true";
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var local = (LocalDeclarationStatementSyntax)((GlobalStatementSyntax)root.Members[0]).Statement!;
        var typeSyntax = local.Declaration.Declarators[0].TypeAnnotation!.Type;
        var union = Assert.IsType<UnionTypeSyntax>(typeSyntax);
        Assert.IsType<LiteralTypeSyntax>(union.Types[0]);
        Assert.IsType<LiteralTypeSyntax>(union.Types[1]);
    }
}
