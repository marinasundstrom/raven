using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class ImportedNullableValueTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    [Trait("Target", "DotNet")]
    public void DotNet_ImportedNullableMembersSupportValueOperations(bool diagnosticsFirst)
        => Verify(TargetPlatform.DotNet, diagnosticsFirst);

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    [Trait("Target", "NeoCLR")]
    public void NeoCLR_ImportedNullableMembersRetainSemanticShape(bool diagnosticsFirst)
        => Verify(TargetPlatform.NeoCLR, diagnosticsFirst);

    private static void Verify(TargetPlatform target, bool diagnosticsFirst)
    {
        var tree = SyntaxTree.ParseText("""
            import Raven.CodeAnalysis.Tests.*
            class Probe {
                static func Equal(value: NullableValueFixture) -> bool => value.Value == value.Field
                static func Text(value: NullableValueFixture) -> string => NullableValueFixture.Echo(value.Value)?.ToString() ?? "none"
                static func Generic(value: NullableValueBox<int>) -> int => value.Value ?? -1
                static func Element(value: NullableValueFixture) -> int => value.Values[0] ?? -1
            }
            """);
        var compilation = Compilation.Create("ImportedNullableValues", [tree], References(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target)
                .WithAllowNullableValueTypes(target == TargetPlatform.DotNet));
        var model = compilation.GetSemanticModel(tree);
        if (diagnosticsFirst)
            Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var fixture = compilation.GetTypeByMetadataName(typeof(NullableValueFixture).FullName!)!;
        Assert.True(Assert.IsAssignableFrom<PropertySymbol>(fixture.GetMembers("Value").Single()).Type.IsNullable);
        Assert.True(Assert.IsAssignableFrom<FieldSymbol>(fixture.GetMembers("Field").Single()).Type.IsNullable);
        var echo = Assert.IsAssignableFrom<MethodSymbol>(fixture.GetMembers("Echo").Single());
        Assert.True(echo.ReturnType.IsNullable);
        Assert.True(Assert.Single(echo.Parameters).Type.IsNullable);
        var values = Assert.IsAssignableFrom<PropertySymbol>(fixture.GetMembers("Values").Single());
        Assert.True(Assert.IsAssignableFrom<ArrayTypeSymbol>(values.Type).ElementType.IsNullable);
        Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    internal static MetadataReference[] References()
        => [.. TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"))
                .Select(MetadataReference.CreateFromFile),
            MetadataReference.CreateFromFile(typeof(NullableValueFixture).Assembly.Location)];
}

public sealed class NullableValueFixture
{
    public int? Field;
    public int? Value { get; set; }
    public int?[] Values { get; set; } = [];
    public static int? Echo(int? value) => value;
}

public sealed class NullableValueBox<T> where T : struct
{
    public T? Value { get; set; }
}
