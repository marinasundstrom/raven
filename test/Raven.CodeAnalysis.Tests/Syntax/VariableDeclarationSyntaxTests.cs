using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class VariableDeclarationSyntaxTests
{
    [Fact]
    public void LocalDeclaration_WithMultipleDeclarators_Parses()
    {
        const string source = """
var a = 1, b = 2
""";

        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var local = Assert.IsType<LocalDeclarationStatementSyntax>(((GlobalStatementSyntax)root.Members[0]).Statement);

        Assert.Equal(SyntaxKind.VarKeyword, local.Declaration.BindingKeyword.Kind);
        Assert.Equal(2, local.Declaration.Declarators.Count);
        Assert.Equal("a", local.Declaration.Declarators[0].Identifier.ValueText);
        Assert.Equal("b", local.Declaration.Declarators[1].Identifier.ValueText);
    }

    [Fact]
    public void LocalDeclaration_WithMultipleDeclaratorsAndTypeAnnotation_Parses()
    {
        const string source = """
val a: int = 1, b: int = 2
""";

        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var local = Assert.IsType<LocalDeclarationStatementSyntax>(((GlobalStatementSyntax)root.Members[0]).Statement);

        Assert.Equal(SyntaxKind.ValKeyword, local.Declaration.BindingKeyword.Kind);
        Assert.Equal(2, local.Declaration.Declarators.Count);
        Assert.All(local.Declaration.Declarators, d => Assert.NotNull(d.TypeAnnotation));
        Assert.True(local.Declaration.Declarators.All(d => d.Initializer is not null));
    }
}
