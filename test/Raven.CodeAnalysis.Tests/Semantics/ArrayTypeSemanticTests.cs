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
        const string source = "let values: System.String[] = []";
        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();

        var typeInfo = model.GetTypeInfo(declarator.TypeAnnotation!.Type);
        var arrayType = Assert.IsAssignableFrom<IArrayTypeSymbol>(typeInfo.Type);
        Assert.Equal(1, arrayType.Rank);
        Assert.Equal(SpecialType.System_String, arrayType.ElementType.SpecialType);
    }

}
