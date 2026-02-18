using System.Linq;

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
    public void ClassDeclaration_WithTypeParameters_ParsesTypeParameterList()
    {
        var source = "class Box<TValue, TOther> {}";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<ClassDeclarationSyntax>(Assert.Single(root.Members));

        Assert.NotNull(declaration.TypeParameterList);
        Assert.Equal(2, declaration.TypeParameterList!.Parameters.Count);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void InterfaceDeclaration_WithVariantTypeParameters_ParsesVarianceModifiers()
    {
        var source = "interface Producer<out T, in U> {}";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<InterfaceDeclarationSyntax>(Assert.Single(root.Members));
        var parameters = declaration.TypeParameterList!.Parameters;

        Assert.True(parameters[0].VarianceKeyword.HasValue);
        Assert.Equal(SyntaxKind.OutKeyword, parameters[0].VarianceKeyword!.Value.Kind);
        Assert.True(parameters[1].VarianceKeyword.HasValue);
        Assert.Equal(SyntaxKind.InKeyword, parameters[1].VarianceKeyword!.Value.Kind);
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

    [Fact]
    public void ClassDeclaration_WithAttributeList_ParsesAttributes()
    {
        var source = "[Obsolete] class Widget {}";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<ClassDeclarationSyntax>(Assert.Single(root.Members));

        var attributeList = Assert.Single(declaration.AttributeLists);
        var attribute = Assert.Single(attributeList.Attributes);

        var name = Assert.IsType<IdentifierNameSyntax>(attribute.Name);
        Assert.Equal("Obsolete", name.Identifier.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void MethodDeclaration_WithAttributeList_ParsesAttributes()
    {
        var source = """
            class Widget {
                [Inline]
                public Do() -> unit {}
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();

        var attributeList = Assert.Single(method.AttributeLists);
        var attribute = Assert.Single(attributeList.Attributes);

        var name = Assert.IsType<IdentifierNameSyntax>(attribute.Name);
        Assert.Equal("Inline", name.Identifier.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void SelfMember_WithGenericReturnType_ParsesWithoutSyntaxErrors()
    {
        var source = """
            class Foo {
                public self(flag: bool) -> Task<Option<int>> {
                    return Test(flag)
                }
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var declaration = Assert.Single(root.Members.OfType<ClassDeclarationSyntax>());

        Assert.NotNull(declaration);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void MethodDeclaration_WithArrowAndMissingReturnType_ReportsIdentifierExpected()
    {
        var source = """
            class Foo {
                public M() -> {}
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();

        Assert.NotNull(method.ReturnType);
        var typeSyntax = Assert.IsType<IdentifierNameSyntax>(method.ReturnType!.Type);
        Assert.True(typeSyntax.Identifier.IsMissing);

        var diagnostic = Assert.Single(tree.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.IdentifierExpected.Id, diagnostic.Descriptor.Id);
    }
}
