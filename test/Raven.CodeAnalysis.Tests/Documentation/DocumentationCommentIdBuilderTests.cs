using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Documentation;

public sealed class DocumentationCommentIdBuilderTests : CompilationTestBase
{
    [Fact]
    public void SourceField_UsesSourceNameAsMetadataName()
    {
        var (compilation, tree) = CreateCompilation("""
            class Widget {
                /// Stores the current count.
                field count: int = 0
            }
            """);

        var fieldDeclaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<FieldDeclarationSyntax>()
            .Single();
        var declarator = fieldDeclaration.Declaration.Declarators.Single();
        var field = Assert.IsAssignableFrom<IFieldSymbol>(
            compilation.GetSemanticModel(tree).GetDeclaredSymbol(declarator));

        Assert.Equal("count", field.MetadataName);
        Assert.Equal(
            "F:Widget.count",
            DocumentationCommentIdBuilder.GetFieldMemberId(field));
    }

    [Fact]
    public void NullableSourceParameter_UsesUnderlyingMetadataType()
    {
        var (compilation, tree) = CreateCompilation("""
            class Parser {
                /// Attempts to parse text.
                static func TryParse(input: string?) -> bool => false
            }
            """);

        var methodDeclaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single();
        var method = Assert.IsAssignableFrom<IMethodSymbol>(
            compilation.GetSemanticModel(tree).GetDeclaredSymbol(methodDeclaration));

        Assert.Equal(
            "M:Parser.TryParse(System.String)",
            DocumentationCommentIdBuilder.GetMethodMemberId(method));
    }

    [Fact]
    public void GenericExtensionMethod_UsesEmittedMetadataShape()
    {
        var (compilation, _) = CreateCompilation("""
            class Box<T> {}

            extension BoxExtensions<T> for Box<T> {
                /// Returns the supplied text.
                func Echo(text: string) -> string => text
            }
            """);

        var extensionType = compilation.GetTypeByMetadataName("BoxExtensions`1");
        Assert.NotNull(extensionType);

        var method = Assert.Single(extensionType!.GetMembers("Echo").OfType<IMethodSymbol>());
        Assert.Equal(
            "M:BoxExtensions.Echo``1(Box{``0},System.String)",
            DocumentationCommentIdBuilder.GetMethodMemberId(method));
    }
}
