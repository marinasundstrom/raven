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
record struct Point
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

    [Theory]
    [InlineData("public", Accessibility.Public)]
    [InlineData("internal", Accessibility.Internal)]
    [InlineData("private", Accessibility.Private)]
    public void RecordStructDeclaration_PrimaryConstructorAccessibility_AppliesToSynthesizedConstructor(
        string modifier,
        Accessibility expectedAccessibility)
    {
        var source = $"record struct Year {modifier} (Value: int) {{}}";
        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<RecordDeclarationSyntax>().Single();
        var year = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(declaration));
        var constructor = Assert.Single(year.InstanceConstructors.Where(static constructor =>
            constructor.Parameters.Length == 1 &&
            constructor.Parameters[0].Type.SpecialType == SpecialType.System_Int32));

        Assert.Equal(expectedAccessibility, constructor.DeclaredAccessibility);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void RecordStructDeclaration_PrivatePrimaryConstructor_IsAccessibleFromFactoryOnly()
    {
        const string source = """
record struct Year private (Value: int) {
    static func Create(value: int) -> Year => Year(value)
}

val invalid = Year(2026)
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();
        var inaccessible = Assert.Single(diagnostics.Where(static diagnostic =>
            diagnostic.Descriptor == CompilerDiagnostics.SymbolIsInaccessible));

        Assert.Equal(4, inaccessible.Location.GetLineSpan().StartLinePosition.Line);
        Assert.DoesNotContain(diagnostics, diagnostic =>
            diagnostic.Severity == DiagnosticSeverity.Error && diagnostic != inaccessible);
    }

    [Fact]
    public void RecordStructDeclaration_ProtectedPrimaryConstructor_ReportsDiagnostic()
    {
        const string source = "record struct Year protected (Value: int) {}";
        var (compilation, _) = CreateCompilation(source);

        Assert.Contains(compilation.GetDiagnostics(), static diagnostic =>
            diagnostic.Descriptor == CompilerDiagnostics.ModifierNotValidOnMember);
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
