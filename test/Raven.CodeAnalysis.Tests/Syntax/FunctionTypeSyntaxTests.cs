using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class FunctionTypeSyntaxTests
{
    [Fact]
    public void FunctionType_SingleParameter_OmitsParentheses()
    {
        var code = "let f: string -> int = s => 0";
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var local = (LocalDeclarationStatementSyntax)((GlobalStatementSyntax)root.Members[0]).Statement!;
        var functionType = Assert.IsType<FunctionTypeSyntax>(local.Declaration.Declarators[0].TypeAnnotation!.Type);

        Assert.NotNull(functionType.Parameter);
        Assert.Null(functionType.ParameterList);
        Assert.Equal("string", functionType.Parameter!.ToString());
        Assert.Equal("int", functionType.ReturnType.ToString());
    }

    [Fact]
    public void FunctionType_WithParameterList_ParsesAllParameters()
    {
        var code = "let f: (int, string) -> bool = (i, s) => true";
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var local = (LocalDeclarationStatementSyntax)((GlobalStatementSyntax)root.Members[0]).Statement!;
        var functionType = Assert.IsType<FunctionTypeSyntax>(local.Declaration.Declarators[0].TypeAnnotation!.Type);

        Assert.Null(functionType.Parameter);
        Assert.NotNull(functionType.ParameterList);
        Assert.Equal(2, functionType.ParameterList!.Parameters.Count);
        Assert.Equal("int", functionType.ParameterList.Parameters[0].ToString());
        Assert.Equal("string", functionType.ParameterList.Parameters[1].ToString());
        Assert.Equal("bool", functionType.ReturnType.ToString());
    }

    [Fact]
    public void FunctionType_Parameterless_Parses()
    {
        var code = "let f: () -> unit = () => ()";
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var local = (LocalDeclarationStatementSyntax)((GlobalStatementSyntax)root.Members[0]).Statement!;
        var functionType = Assert.IsType<FunctionTypeSyntax>(local.Declaration.Declarators[0].TypeAnnotation!.Type);

        Assert.Null(functionType.Parameter);
        Assert.NotNull(functionType.ParameterList);
        Assert.Empty(functionType.ParameterList!.Parameters);
        Assert.Equal("unit", functionType.ReturnType.ToString());
    }
}
