using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class NullableValueMemberTests
{
    [Fact]
    public void NullableSpecialType_ResolvesGenericDefinition()
    {
        var compilation = Compilation.Create("NullableDefinition", [], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var definition = compilation.GetSpecialType(SpecialType.System_Nullable_T);
        Assert.Equal(1, definition.Arity);
        Assert.Equal(SpecialType.System_Nullable_T, definition.SpecialType);
    }

    [Theory]
    [InlineData("GetValueOrDefault()", 0)]
    [InlineData("GetValueOrDefault(7)", 7)]
    public void NullableRuntimeMethods_ExecuteForPresentAndAbsentValues(string call, int absent)
    {
        var tree = SyntaxTree.ParseText($$"""
            class Probe { static func Read(value: int?) -> int => value.{{call}} }
            """);
        var compilation = Compilation.Create("NullableMembers", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var method = loaded.Assembly.GetType("Probe")!.GetMethod("Read")!;
        Assert.Equal(absent, method.Invoke(null, new object?[] { null }));
        Assert.Equal(42, method.Invoke(null, new object?[] { 42 }));
    }

    [Fact]
    public void NullableRuntimeProperties_ExecuteOnTheWrapper()
    {
        var tree = SyntaxTree.ParseText("""
            class Probe {
                static func Present(value: int?) -> bool => value.HasValue
                static func Read(value: int?) -> int => value.Value
            }
            """);
        var compilation = Compilation.Create("NullableProperties", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var type = loaded.Assembly.GetType("Probe")!;
        Assert.Equal(false, type.GetMethod("Present")!.Invoke(null, new object?[] { null }));
        Assert.Equal(true, type.GetMethod("Present")!.Invoke(null, new object?[] { 42 }));
        Assert.Equal(42, type.GetMethod("Read")!.Invoke(null, new object?[] { 42 }));
        var failure = Assert.Throws<System.Reflection.TargetInvocationException>(() => type.GetMethod("Read")!.Invoke(null, new object?[] { null }));
        Assert.IsType<InvalidOperationException>(failure.InnerException);
    }
    [Theory]
    [InlineData("GetValueOrDefault()", "GetValueOrDefault")]
    [InlineData("HasValue", "HasValue")]
    public void NullableMembers_ResolveThroughColdSemanticQueries(string expression, string name)
    {
        var tree = SyntaxTree.ParseText($$"""
            class Probe { static func Read(value: int?) => value.{{expression}} }
            """);
        var compilation = Compilation.Create("NullableSymbols", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var member = tree.GetRoot().DescendantNodes().OfType<MemberAccessExpressionSyntax>().Single();
        var model = compilation.GetSemanticModel(tree);
        var symbol = model.GetSymbolInfo(member).Symbol;
        Assert.NotNull(symbol);
        Assert.Equal(name, symbol.Name);
        Assert.Equal(SpecialType.System_Nullable_T, symbol.ContainingType!.SpecialType);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void NullableSourceStruct_DefaultValuePreservesValueSemantics()
    {
        var tree = SyntaxTree.ParseText("""
            struct Item<T> { public var Number: int }
            class Probe {
                static func Read<T>(value: Item<T>?) -> int => value.GetValueOrDefault().Number
            }
            """);
        var compilation = Compilation.Create("NullableStruct", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(0, loaded.Assembly.GetType("Probe")!.GetMethod("Read")!.MakeGenericMethod(typeof(string)).Invoke(null, new object?[] { null }));
    }

    [Theory]
    [InlineData("int?", "CompareTo(1)")]
    [InlineData("string?", "Length")]
    public void NullableContainedMembers_StillRequireUnwrapping(string type, string expression)
    {
        var tree = SyntaxTree.ParseText($$"""
            class Probe { static func Read(value: {{type}}) => value.{{expression}} }
            """);
        var compilation = Compilation.Create("NullableContainedMembers", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV0402");
    }

}
