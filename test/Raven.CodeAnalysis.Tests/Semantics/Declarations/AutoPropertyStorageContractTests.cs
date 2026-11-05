using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class AutoPropertyStorageContractTests
{
    [Theory]
    [InlineData("val Value: int { get; }", true)]
    [InlineData("val Value: int { get; init; }", true)]
    [InlineData("var Value: int { get; set; }", false)]
    [Trait("Target", "DotNet")]
    public void DotNet_AutoPropertyStorageReflectsAccessorMutability(string declaration, bool readOnly)
        => Verify(TargetPlatform.DotNet, declaration, readOnly);

    [Theory]
    [InlineData("val Value: int { get; }", true)]
    [InlineData("val Value: int { get; init; }", true)]
    [InlineData("var Value: int { get; set; }", false)]
    [Trait("Target", "NeoCLR")]
    public void NeoCLR_AutoPropertyStorageReflectsAccessorMutability(string declaration, bool readOnly)
        => Verify(TargetPlatform.NeoCLR, declaration, readOnly);

    private static void Verify(TargetPlatform target, string declaration, bool readOnly)
    {
        var tree = SyntaxTree.ParseText($"class Data {{ {declaration} }}");
        var compilation = Compilation.Create("StorageContract", [tree],
            TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"))
                .Select(MetadataReference.CreateFromFile).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
        // These are semantic checks with CLI references, not native-core emission.
        var model = compilation.GetSemanticModel(tree);
        Assert.DoesNotContain(model.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();
        var property = Assert.IsType<SourcePropertySymbol>(model.GetDeclaredSymbol(syntax));
        Assert.NotNull(property.BackingField);
        Assert.Equal(readOnly, property.BackingField!.IsReadOnly);
    }
}
