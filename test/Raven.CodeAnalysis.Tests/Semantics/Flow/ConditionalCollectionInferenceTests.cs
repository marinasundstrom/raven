using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class ConditionalCollectionInferenceTests
{
    [Theory]
    [InlineData(false, false, false)]
    [InlineData(false, true, false)]
    [InlineData(true, false, false)]
    [InlineData(true, true, false)]
    [InlineData(false, false, true)]
    [InlineData(true, false, true)]
    public void EmptyBranch_UsesOtherBranchArrayType(bool emptyFirst, bool diagnosticsFirst, bool collectionFirst)
    {
        var source = $$"""
func Pick(flag: bool, values: int[]) -> int[] {
    let result = if flag { {{(emptyFirst ? "[]" : "values")}} } else { {{(emptyFirst ? "values" : "[]")}} }
    return result
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("ConditionalCollection", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();
        var model = compilation.GetSemanticModel(tree);
        if (collectionFirst)
        {
            var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();
            Assert.IsAssignableFrom<ArrayTypeSymbol>(model.GetTypeInfo(collection).Type);
        }
        var expression = tree.GetRoot().DescendantNodes().OfType<IfExpressionSyntax>().Single();
        var type = Assert.IsAssignableFrom<ArrayTypeSymbol>(model.GetTypeInfo(expression).Type);
        Assert.Equal(SpecialType.System_Int32, type.ElementType.SpecialType);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void EmptyBranch_PreservesPatternScope(bool usePatternInElse)
    {
        var tree = SyntaxTree.ParseText($$"""
func Pick(value: object, values: int[]) -> int[] {
    let result = if value is int number { let copy = number; [] } else { {{(usePatternInElse ? "let invalid = number;" : "")}} values }
    return result
}
""");
        var compilation = Compilation.Create("ConditionalPatternScope", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (usePatternInElse)
            Assert.Contains(errors, d => d.Id == "RAV0103");
        else
            Assert.Empty(errors);
    }

    [Fact]
    public void BothEmptyBranches_StillRequireAContext()
    {
        var tree = SyntaxTree.ParseText("func Pick(flag: bool) -> unit { let result = if flag { [] } else { [] } }");
        var compilation = Compilation.Create("ConditionalEmpty", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Descriptor == CompilerDiagnostics.EmptyCollectionLiteralRequiresTargetType);
    }

    [Fact]
    public void FormatOverload_UsesConditionalArrayArgument()
    {
        var tree = SyntaxTree.ParseText("""
func Format(flag: bool, values: object?[]) -> string {
    return string.Format("{0}", if flag { values } else { [] })
}
""");
        var compilation = Compilation.Create("ConditionalFormat", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

}

[Trait("Target", "NeoCLR")]
public sealed class NeoClrConditionalCollectionInferenceTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void NeoClrSemanticBindingWithHostReferences_InfersEmptyBranch(bool emptyFirst)
    {
        var tree = SyntaxTree.ParseText($$"""
func Pick(flag: bool, values: int[]) -> int[] {
    let result = if flag { {{(emptyFirst ? "[]" : "values")}} } else { {{(emptyFirst ? "values" : "[]")}} }
    return result
}
""");
        var compilation = Compilation.Create("NeoClrConditionalCollection", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(TargetPlatform.NeoCLR));
        var model = compilation.GetSemanticModel(tree);
        var expression = tree.GetRoot().DescendantNodes().OfType<IfExpressionSyntax>().Single();
        Assert.IsAssignableFrom<ArrayTypeSymbol>(model.GetTypeInfo(expression).Type);
        Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }
}
