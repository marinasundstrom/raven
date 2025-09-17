using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class InterfaceDeclarationParserTests
{
    [Fact]
    public void InterfaceDeclaration_AllowsEmptyBody()
    {
        var source = "interface IA {}";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<InterfaceDeclarationSyntax>(Assert.Single(root.Members));

        Assert.Equal("IA", declaration.Identifier.Text);
        Assert.Empty(declaration.Members);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void InterfaceDeclaration_AllowsAdjacentDeclarations()
    {
        var source = "interface IA {} interface IB {} class C : IA, IB {}";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var members = root.Members;

        Assert.Equal(3, members.Count);
        Assert.IsType<InterfaceDeclarationSyntax>(members[0]);
        Assert.IsType<InterfaceDeclarationSyntax>(members[1]);

        var classDeclaration = Assert.IsType<ClassDeclarationSyntax>(members[2]);
        Assert.NotNull(classDeclaration.BaseList);
        var baseList = classDeclaration.BaseList!;
        Assert.Equal(2, baseList.Types.Count);

        Assert.Empty(tree.GetDiagnostics());
    }
}
