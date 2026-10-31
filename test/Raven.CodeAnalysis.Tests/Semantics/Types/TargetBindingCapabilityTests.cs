using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class TargetBindingCapabilityTests
{
    [Fact]
    [Trait("Target", "DotNet")]
    public void DotNetDefaultKeepsReturnTypeAsAnOrdinaryGenericName()
    {
        var compilation = Create("""
            class ReturnType<T> {}
            class Consumer { static func Read(value: ReturnType<int>) -> ReturnType<int> => value }
            """);
        Assert.Equal(TargetPlatform.DotNet, compilation.Options.TargetPlatform);
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var method = Assert.Single(compilation.GetTypeByMetadataName("Consumer")!.GetMembers("Read").OfType<IMethodSymbol>());
        var result = Assert.IsAssignableFrom<INamedTypeSymbol>(method.ReturnType);
        Assert.Equal("ReturnType", result.Name);
        Assert.Equal(SpecialType.System_Int32, Assert.Single(result.TypeArguments).SpecialType);
        Assert.True(SymbolEqualityComparer.Default.Equals(result, method.Parameters[0].Type));
    }

    [Fact]
    [Trait("Target", "NeoCLR")]
    public void NeoClrProjectsStructuralFunctionReturnType()
    {
        var compilation = Create("""
            class Consumer { static func Read(value: ReturnType<() -> int>) -> int => value }
            """, TargetPlatform.NeoCLR);
        var tree = compilation.SyntaxTrees.Single();
        var model = compilation.GetSemanticModel(tree);
        Assert.Empty(model.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var syntax = tree.GetRoot().DescendantNodes().OfType<GenericNameSyntax>().Single();
        Assert.Equal(SpecialType.System_Int32, model.GetTypeInfo(syntax).Type!.SpecialType);
    }

    [Fact]
    [Trait("Target", "DotNet")]
    public void DotNetDefaultCharBindingAgreesWithPredefinedTypeApi()
    {
        var compilation = Create("class Consumer { static func Read(value: char) -> char => value }");
        var tree = compilation.SyntaxTrees.Single();
        var syntax = tree.GetRoot().DescendantNodes().OfType<PredefinedTypeSyntax>().First();
        var type = compilation.GetSemanticModel(tree).GetTypeInfo(syntax).Type;
        Assert.Equal(SpecialType.System_Char, type!.SpecialType);
        Assert.Same(compilation.ResolvePredefinedType(syntax), type);
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
    }

    [Fact]
    [Trait("Target", "NeoCLR")]
    public void NeoClrRejectsCharKeywordInBindingAndPredefinedTypeApi()
    {
        var compilation = Create("class Consumer { static func Read(value: char) {} }", TargetPlatform.NeoCLR);
        var tree = compilation.SyntaxTrees.Single();
        var syntax = tree.GetRoot().DescendantNodes().OfType<PredefinedTypeSyntax>().Single();
        var model = compilation.GetSemanticModel(tree);
        var type = model.GetTypeInfo(syntax).Type;
        Assert.True(type is null || type.TypeKind == TypeKind.Error);
        Assert.Equal(TypeKind.Error, compilation.ResolvePredefinedType(syntax).TypeKind);
        Assert.Contains(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error && d.Location.SourceSpan == syntax.Span);
    }

    private static Compilation Create(string source, TargetPlatform? target = null)
    {
        // Semantic-only native fixtures use host reference symbols, not a native
        // runtime core. These tests do not qualify native emission or execution.
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary);
        if (target is { } platform)
            options = options.WithTargetPlatform(platform);
        return Compilation.Create("BindingCapabilities", [SyntaxTree.ParseText(source)], TestMetadataReferences.Default, options);
    }
}
