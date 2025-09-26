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
                public static Value: int {
                    get => 0
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
                IFoo.Value: int {
                    get => 0
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
                public Value: int {
                    [Getter]
                    get => 0
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
}
