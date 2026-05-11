using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class PropertySemanticTests : CompilationTestBase
{
    [Fact]
    public void AutoProperty_HasAccessorSymbolsAndBackingField()
    {
        const string source = """
            class Counter {
                public var Count: int
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        _ = compilation.GetDiagnostics();
        var model = compilation.GetSemanticModel(tree);
        var classDeclaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var counter = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(classDeclaration));
        var property = Assert.IsAssignableFrom<SourcePropertySymbol>(counter.GetMembers("Count").OfType<IPropertySymbol>().Single());

        Assert.NotNull(property.GetMethod);
        Assert.NotNull(property.SetMethod);
        Assert.Equal(MethodKind.PropertyGet, property.GetMethod!.MethodKind);
        Assert.Equal(MethodKind.PropertySet, property.SetMethod!.MethodKind);

        var backingField = Assert.IsAssignableFrom<IFieldSymbol>(property.BackingField);
        Assert.Equal("<Count>k__BackingField", backingField.Name);
        Assert.False(backingField.IsStatic);
        Assert.Equal(SpecialType.System_Int32, backingField.Type.SpecialType);
    }

    [Fact]
    public void StaticAutoProperty_HasAccessorSymbolsAndBackingField()
    {
        const string source = """
            class Counter {
                public static var Count: int
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();
        _ = compilation.GetDiagnostics();
        var model = compilation.GetSemanticModel(tree);
        var classDeclaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var counter = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(classDeclaration));
        var property = Assert.IsAssignableFrom<SourcePropertySymbol>(counter.GetMembers("Count").OfType<IPropertySymbol>().Single());

        Assert.True(property.IsStatic);
        Assert.NotNull(property.GetMethod);
        Assert.NotNull(property.SetMethod);
        Assert.True(property.GetMethod!.IsStatic);
        Assert.True(property.SetMethod!.IsStatic);

        var backingField = Assert.IsAssignableFrom<IFieldSymbol>(property.BackingField);
        Assert.Equal("<Count>k__BackingField", backingField.Name);
        Assert.True(backingField.IsStatic);
        Assert.Equal(SpecialType.System_Int32, backingField.Type.SpecialType);
    }
}
