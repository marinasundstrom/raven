using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class UnionTypeSyntaxTest
{
    [Fact]
    public void UnionType_WithTwoAlternatives_Parses()
    {
        var code = "func accept(value: int | string) -> () { }";
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var function = (FunctionStatementSyntax)((GlobalStatementSyntax)root.Members[0]).Statement!;
        var typeSyntax = function.ParameterList.Parameters[0].TypeAnnotation!.Type;

        var union = Assert.IsType<UnionTypeSyntax>(typeSyntax);
        Assert.Equal(2, union.Types.Count);
        Assert.IsType<PredefinedTypeSyntax>(union.Types[0]);
        Assert.IsType<PredefinedTypeSyntax>(union.Types[1]);
    }

    [Fact]
    public void UnionType_WithFunctionAlternative_KeepsFunctionTypeAsElement()
    {
        var code = "func accept(value: int | string -> bool) -> () { }";
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var function = (FunctionStatementSyntax)((GlobalStatementSyntax)root.Members[0]).Statement!;
        var typeSyntax = function.ParameterList.Parameters[0].TypeAnnotation!.Type;

        var union = Assert.IsType<UnionTypeSyntax>(typeSyntax);
        Assert.Equal(2, union.Types.Count);
        Assert.IsType<PredefinedTypeSyntax>(union.Types[0]);
        Assert.IsType<FunctionTypeSyntax>(union.Types[1]);
    }
}
