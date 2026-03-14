using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class PropertyDeclarationSyntaxTest : DiagnosticTestBase
{
    [Fact]
    public void StaticProperty()
    {
        string testCode =
            """
            class Foo {
                static val Value: int {
                    get { return 0 }
                }
            }
            """;

        var verifier = CreateVerifier(testCode);
        verifier.Verify();
    }

    [Fact]
    public void ExplicitInterfaceProperty_UsesSpecifierIdentifier()
    {
        const string code =
            """
            class Foo {
                val IFoo.Value: int {
                    get { return 0 }
                }
            }
            """;

        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();

        var property = root.DescendantNodes()
            .OfType<PropertyDeclarationSyntax>()
            .Single(p => p.ExplicitInterfaceSpecifier is not null);

        var explicitInterface = property.ExplicitInterfaceSpecifier;
        Assert.NotNull(explicitInterface);
        Assert.Equal(SyntaxKind.None, property.Identifier.Kind);
        Assert.Equal("Value", explicitInterface!.Identifier.Text);
        Assert.Equal("IFoo", explicitInterface.Name.ToString());
    }

    [Fact]
    public void AccessorDeclaration_WithAttributeList_ParsesAttributes()
    {
        string testCode =
            """
            class Foo {
                val Value: int {
                    [Getter]
                    get { return 0 }
                }
            }
            """;

        var tree = SyntaxTree.ParseText(testCode);
        var accessor = tree.GetRoot()
            .DescendantNodes()
            .OfType<AccessorDeclarationSyntax>()
            .Single(a => a.Kind == SyntaxKind.GetAccessorDeclaration);

        var attributeList = Assert.Single(accessor.AttributeLists);
        var attribute = Assert.Single(attributeList.Attributes);

        var name = Assert.IsType<IdentifierNameSyntax>(attribute.Name);
        Assert.Equal("Getter", name.Identifier.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void PropertyWithoutType_ProducesMissingTypeAnnotation()
    {
        const string code =
            """
            class Foo {
                val Value {
                    get { return 0 }
                }
            }
            """;

        var tree = SyntaxTree.ParseText(code);
        var property = tree.GetRoot()
            .DescendantNodes()
            .OfType<PropertyDeclarationSyntax>()
            .Single();

        Assert.True(property.Type.ColonToken.IsMissing);
        var typeSyntax = Assert.IsType<IdentifierNameSyntax>(property.Type.Type);
        Assert.True(typeSyntax.Identifier.IsMissing);
    }

    [Fact]
    public void PropertyWithInitializer_ParsesEqualsClause()
    {
        const string code =
            """
            struct Token {
                val Kind: SyntaxKind { get } = kind
            }
            """;

        var tree = SyntaxTree.ParseText(code);
        var property = tree.GetRoot()
            .DescendantNodes()
            .OfType<PropertyDeclarationSyntax>()
            .Single();

        var initializer = Assert.IsType<EqualsValueClauseSyntax>(property.Initializer);
        Assert.Equal(SyntaxKind.EqualsToken, initializer.EqualsToken.Kind);
        var valueExpression = Assert.IsType<IdentifierNameSyntax>(initializer.Value);
        Assert.Equal("kind", valueExpression.Identifier.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void SameLineAutoAccessors_PreserveSemicolonTerminator()
    {
        const string code =
            """
            class Foo {
                var Value: int { get; set }
            }
            """;

        var tree = SyntaxTree.ParseText(code);
        var property = tree.GetRoot()
            .DescendantNodes()
            .OfType<PropertyDeclarationSyntax>()
            .Single();

        var accessors = property.AccessorList!.Accessors;
        Assert.Equal(SyntaxKind.SemicolonToken, accessors[0].TerminatorToken.Kind);
        Assert.Equal(SyntaxKind.None, accessors[1].TerminatorToken.Kind);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void SameLineAccessors_WithoutSemicolon_InsertMissingTerminator()
    {
        const string code =
            """
            class Foo {
                var Value: int { get set }
            }
            """;

        var tree = SyntaxTree.ParseText(code);
        var property = tree.GetRoot()
            .DescendantNodes()
            .OfType<PropertyDeclarationSyntax>()
            .Single();

        var accessors = property.AccessorList!.Accessors;
        Assert.True(accessors[0].TerminatorToken.IsMissing);
        Assert.Equal(SyntaxKind.SemicolonToken, accessors[0].TerminatorToken.Kind);
    }
}
