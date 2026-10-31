using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class ArraySliceTargetTests
{
    [Fact]
    [Trait("Target", "DotNet")]
    public void DefaultTargetSelectsArrayCopyEvenWhenNativeHelperExists()
    {
        var (compilation, access) = Create();
        Assert.Equal(TargetPlatform.DotNet, compilation.Options.TargetPlatform);
        Assert.Empty(compilation.GetSemanticModel(access.SyntaxTree).GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var model = compilation.GetSemanticModel(access.SyntaxTree);
        var type = Assert.IsAssignableFrom<IArrayTypeSymbol>(model.GetTypeInfo(access).Type);
        Assert.Equal(SpecialType.System_Int32, type.ElementType.SpecialType);
        Assert.Equal("GetSubArray", Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(access).Symbol).Name);
    }

    [Fact]
    [Trait("Target", "NeoCLR")]
    public void NeoClrSelectsRefLikeViewEvenWhenDotNetHelperExists()
    {
        var (compilation, access) = Create(TargetPlatform.NeoCLR);
        Assert.Empty(compilation.GetSemanticModel(access.SyntaxTree).GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var model = compilation.GetSemanticModel(access.SyntaxTree);
        var type = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetTypeInfo(access).Type);
        Assert.True(type.IsRefLikeType);
        Assert.Equal("System.Slice`1", type.OriginalDefinition.ToFullyQualifiedMetadataName());
        Assert.Equal(SpecialType.System_Int32, Assert.Single(type.TypeArguments).SpecialType);
        Assert.Equal("GetSlice", Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(access).Symbol).Name);
    }

    [Theory]
    [Trait("Target", "NeoCLR")]
    [InlineData("struct Slice<T> {}", "System.Slice<T>", "GetSlice")]
    [InlineData("ref struct Other<T> {}", "System.Other<T>", "GetSlice")]
    [InlineData("ref struct Slice<T> {}", "System.Slice<string>", "GetSlice")]
    [InlineData("ref struct Slice<T> {}", "T[]", "GetSlice")]
    [InlineData("ref struct Slice<T> {}", "System.Slice<T>", "MissingHelper")]
    public void NeoClrRejectsInvalidViewContractWithoutFallingBackToArrayCopy(
        string declaration, string returnType, string methodName)
    {
        var (compilation, access) = Create(TargetPlatform.NeoCLR, declaration, returnType, methodName);
        Assert.Contains(compilation.GetSemanticModel(access.SyntaxTree).GetDiagnostics(), d =>
            d.Id == "RAV0021" && d.Severity == DiagnosticSeverity.Error &&
            d.Location.SourceTree == access.SyntaxTree &&
            access.Span.Contains(d.Location.SourceSpan));
        Assert.Null(compilation.GetSemanticModel(access.SyntaxTree).GetSymbolInfo(access).Symbol);
    }

    private static (Compilation Compilation, ElementAccessExpressionSyntax Access) Create(
        TargetPlatform? target = null,
        string declaration = "ref struct Slice<T> {}",
        string returnType = "System.Slice<T>",
        string methodName = "GetSlice")
    {
        var source = SyntaxTree.ParseText("""
            class Consumer {
                static func Use(values: int[]) {
                    val part = values[1..3]
                }
            }
            """);
        var view = SyntaxTree.ParseText("namespace System\n" + declaration);
        var helpers = SyntaxTree.ParseText($$"""
            namespace System.Runtime.CompilerServices
            static class RuntimeHelpers {
                static func GetSubArray<T>(array: T[], range: System.Range) -> T[] => array
                static func {{methodName}}<T>(array: T[], range: System.Range) -> {{returnType}} => default({{returnType}})
            }
            """);
        // Binding-only fixture: host reference symbols supply Index/Range. Native
        // runtime configuration and execution are validated separately, not here.
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary);
        if (target is { } platform)
            options = options.WithTargetPlatform(platform);
        var compilation = Compilation.Create("ArraySliceContract", [source, view, helpers], TestMetadataReferences.Default, options);
        return (compilation, source.GetRoot().DescendantNodes().OfType<ElementAccessExpressionSyntax>().Single());
    }
}
