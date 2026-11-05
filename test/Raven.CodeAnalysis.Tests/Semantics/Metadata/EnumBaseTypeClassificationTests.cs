namespace Raven.CodeAnalysis.Tests;

public sealed class EnumBaseTypeClassificationTests
{
    [Theory]
    [InlineData("System.Enum", false, TypeKind.Class)]
    [InlineData("System.ValueType", false, TypeKind.Class)]
    [InlineData("System.DayOfWeek", true, TypeKind.Enum)]
    [InlineData("System.Int32", true, TypeKind.Struct)]
    [Trait("Target", "DotNet")]
    public void DotNet_ImportedEnumBaseHasReferenceStorage(string name, bool valueType, TypeKind kind)
        => Verify(TargetPlatform.DotNet, name, valueType, kind);

    [Theory]
    [InlineData("System.Enum", false, TypeKind.Class)]
    [InlineData("System.ValueType", false, TypeKind.Class)]
    [InlineData("System.DayOfWeek", true, TypeKind.Enum)]
    [InlineData("System.Int32", true, TypeKind.Struct)]
    [Trait("Target", "NeoCLR")]
    public void NeoCLR_CliImportedEnumBaseHasReferenceStorage(string name, bool valueType, TypeKind kind)
        => Verify(TargetPlatform.NeoCLR, name, valueType, kind);

    private static void Verify(TargetPlatform target, string name, bool valueType, TypeKind kind)
    {
        var references = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"))
            .Select(path => MetadataReference.CreateFromFile(path)).ToArray();
        var compilation = Compilation.Create("EnumClassification", [], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
        var symbol = compilation.GetTypeByMetadataName(name);
        Assert.NotNull(symbol);
        Assert.Equal(valueType, symbol.IsValueType);
        Assert.Equal(kind, symbol.TypeKind);
    }
}
