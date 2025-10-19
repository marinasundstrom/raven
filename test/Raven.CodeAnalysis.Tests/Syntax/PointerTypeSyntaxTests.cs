using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class PointerTypeSyntaxTests
{
    [Fact]
    public void PointerType_InVariableDeclaration_Parses()
    {
        var code = "let ptr: *int = 0";
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var local = (LocalDeclarationStatementSyntax)((GlobalStatementSyntax)root.Members[0]).Statement!;
        var typeSyntax = local.Declaration.Declarators[0].TypeAnnotation!.Type;
        Assert.IsType<PointerTypeSyntax>(typeSyntax);
    }
}
