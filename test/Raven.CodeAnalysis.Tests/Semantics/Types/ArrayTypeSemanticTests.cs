using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ArrayTypeSemanticTests : CompilationTestBase
{
    [Fact]
    public void SingleDimensionalArrayTypeSyntax_BindsToArrayTypeSymbol()
    {
        const string source = "val values: System.String[] = []";
        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();

        var typeInfo = model.GetTypeInfo(declarator.TypeAnnotation!.Type);
        var arrayType = Assert.IsAssignableFrom<IArrayTypeSymbol>(typeInfo.Type);
        Assert.Equal(1, arrayType.Rank);
        Assert.Equal(SpecialType.System_String, arrayType.ElementType.SpecialType);
        Assert.False(arrayType.IsFixedArray);
        Assert.Null(arrayType.FixedLength);
    }

    [Fact]
    public void FixedSizeArrayTypeSyntax_BindsToArrayTypeSymbol()
    {
        const string source = "val values: int[4] = [1, 2, 3, 4]";
        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();

        var typeInfo = model.GetTypeInfo(declarator.TypeAnnotation!.Type);
        var arrayType = Assert.IsAssignableFrom<IArrayTypeSymbol>(typeInfo.Type);
        Assert.Equal(1, arrayType.Rank);
        Assert.True(arrayType.IsFixedArray);
        Assert.Equal(4, arrayType.FixedLength);
    }

    [Fact]
    public void FixedSizeArrayTarget_WithFixedSpreadOfMatchingLength_Binds()
    {
        const string source = """
val values: int[2] = [1, 2]
val result: int[3] = [...values, 3]
""";

        var (compilation, tree) = CreateCompilation(source);
        Assert.Empty(compilation.GetDiagnostics());

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Last();
        var arrayType = Assert.IsAssignableFrom<IArrayTypeSymbol>(model.GetTypeInfo(declarator.TypeAnnotation!.Type).Type);
        Assert.Equal(3, arrayType.FixedLength);
    }
}
