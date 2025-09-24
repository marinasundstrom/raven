using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class ClassDeclarationParserTests : DiagnosticTestBase
{
    [Fact]
    public void ClassDeclaration_WithPrimaryConstructor_ParsesParameterList()
    {
        var source = "class Person(name: string, age: int) {}";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<ClassDeclarationSyntax>(Assert.Single(root.Members));

        Assert.NotNull(declaration.ParameterList);
        Assert.Equal(2, declaration.ParameterList!.Parameters.Count);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void Constructor_WithExpressionBody_ParsesExpressionBody()
    {
        var source = """
            class Person {
                public init()
                    => Console.WriteLine("Hello")
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var ctor = tree.GetRoot().DescendantNodes().OfType<ConstructorDeclarationSyntax>().Single();

        Assert.Null(ctor.Body);
        Assert.NotNull(ctor.ExpressionBody);
        Assert.True(ctor.ExpressionBody!.ArrowToken.IsKind(SyntaxKind.FatArrowToken));
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void Constructor_WithInitializerAndExpressionBody_ParsesBoth()
    {
        var source = """
            class Derived : Base {
                public init(): base(1)
                    => Foo()
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var ctor = tree.GetRoot().DescendantNodes().OfType<ConstructorDeclarationSyntax>().Single();

        Assert.NotNull(ctor.Initializer);
        Assert.NotNull(ctor.ExpressionBody);
        Assert.True(ctor.ExpressionBody!.ArrowToken.IsKind(SyntaxKind.FatArrowToken));
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void NamedConstructor_WithExpressionBody_ParsesExpressionBody()
    {
        var source = """
            class Person {
                public init WithName(name: string) => Person()
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var ctor = tree.GetRoot().DescendantNodes().OfType<NamedConstructorDeclarationSyntax>().Single();

        Assert.Null(ctor.Body);
        Assert.NotNull(ctor.ExpressionBody);
        Assert.True(ctor.ExpressionBody!.ArrowToken.IsKind(SyntaxKind.FatArrowToken));
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void Constructor_WithExpressionBody_FollowedByMethod_ParsesBothMembers()
    {
        var source = """
            class Foo {
                public init() => Console.WriteLine("Init")
                public Dispose() -> unit {}
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var @class = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();

        var members = @class.Members;

        Assert.Collection(
            members,
            member => Assert.IsType<ConstructorDeclarationSyntax>(member),
            member => Assert.IsType<MethodDeclarationSyntax>(member));

        Assert.Empty(tree.GetDiagnostics());
    }
}
