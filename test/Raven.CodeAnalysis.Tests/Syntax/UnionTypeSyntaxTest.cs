using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class UnionTypeSyntaxTest
{
    [Fact]
    public void UnionType_WithNullElement_ParsesLeadingTypeAndReportsUnexpectedPipe()
    {
        var code = "val x: string | null = null";
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var local = (LocalDeclarationStatementSyntax)((GlobalStatementSyntax)root.Members[0]).Statement!;
        var typeSyntax = local.Declaration.Declarators[0].TypeAnnotation!.Type;
        Assert.IsType<PredefinedTypeSyntax>(typeSyntax);
        Assert.Contains(tree.GetDiagnostics(), d => d.Id == "RAV1019");
    }

    [Fact]
    public void UnionType_WithLiteralElements_ParsesLeadingLiteralTypeAndReportsUnexpectedPipe()
    {
        var code = "val flag: true | false = true";
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var local = (LocalDeclarationStatementSyntax)((GlobalStatementSyntax)root.Members[0]).Statement!;
        var typeSyntax = local.Declaration.Declarators[0].TypeAnnotation!.Type;
        Assert.IsType<LiteralTypeSyntax>(typeSyntax);
        Assert.Contains(tree.GetDiagnostics(), d => d.Id == "RAV1019");
    }
}
