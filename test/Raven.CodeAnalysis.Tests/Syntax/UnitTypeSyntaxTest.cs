using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class UnitTypeSyntaxTest
{
    [Fact]
    public void UnitType_InVariableDeclaration_Parses()
    {
        var code = "let x: () = ()";
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var local = (LocalDeclarationStatementSyntax)((GlobalStatementSyntax)root.Members[0]).Statement!;
        var typeSyntax = local.Declaration.Declarators[0].TypeAnnotation!.Type;
        Assert.IsType<UnitTypeSyntax>(typeSyntax);
    }
}
