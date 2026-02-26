using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class StructDeclarationSemanticTests : CompilationTestBase
{
    [Fact]
    public void StructDeclaration_BindsAsValueType()
    {
        const string source = """
struct Point {
    var X: int = 0
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<StructDeclarationSyntax>().Single();
        var point = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(declaration));

        Assert.Equal(TypeKind.Struct, point.TypeKind);
        Assert.Equal(SpecialType.System_ValueType, point.BaseType?.SpecialType);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void RecordStructDeclaration_BindsAsValueType()
    {
        const string source = """
record struct Point {
    var X: int = 0
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<RecordDeclarationSyntax>().Single();
        var point = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(declaration));

        Assert.Equal(TypeKind.Struct, point.TypeKind);
        Assert.Equal(SpecialType.System_ValueType, point.BaseType?.SpecialType);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void RecordStructDeclaration_WithPrimaryConstructor_SynthesizesProperties()
    {
        const string source = """
record struct Point(X: int, Y: int) {}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<RecordDeclarationSyntax>().Single();
        var point = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(declaration));

        Assert.Equal(TypeKind.Struct, point.TypeKind);
        Assert.Contains(point.GetMembers("X"), static member => member is IPropertySymbol);
        Assert.Contains(point.GetMembers("Y"), static member => member is IPropertySymbol);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void RecordStructDeclaration_BaseList_WithClassEntry_DoesNotChangeBaseType()
    {
        const string source = """
record class Entity(Id: int) {}
record struct Point(X: int) : Entity {}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<RecordDeclarationSyntax>()
            .Single(static record => record.Identifier.ValueText == "Point");
        var point = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(declaration));

        Assert.Equal(TypeKind.Struct, point.TypeKind);
        Assert.Equal(SpecialType.System_ValueType, point.BaseType?.SpecialType);
    }
}
