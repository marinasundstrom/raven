using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class RecordDeclarationParserTests : DiagnosticTestBase
{
    [Fact]
    public void RecordClassDeclaration_ParsesRecordDeclarationSyntax()
    {
        var source = "record class Person(name: string, age: int) {}";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<RecordDeclarationSyntax>(Assert.Single(root.Members));

        Assert.NotNull(declaration.ParameterList);
        Assert.Equal(2, declaration.ParameterList!.Parameters.Count);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void RecordDeclaration_WithoutClassKeyword_DefaultsToRecordClass()
    {
        var source = "record Person(name: string, age: int) {}";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<RecordDeclarationSyntax>(Assert.Single(root.Members));

        Assert.NotNull(declaration.ParameterList);
        Assert.Equal(2, declaration.ParameterList!.Parameters.Count);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void RecordStructDeclaration_WithPrimaryConstructor_ParsesParameterList()
    {
        var source = "record struct Point(x: int, y: int) {}";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<StructDeclarationSyntax>(Assert.Single(root.Members));

        Assert.Contains(declaration.Modifiers, modifier => modifier.IsKind(SyntaxKind.RecordKeyword));
        Assert.NotNull(declaration.ParameterList);
        Assert.Equal(2, declaration.ParameterList!.Parameters.Count);
        Assert.Empty(tree.GetDiagnostics());
    }
}
