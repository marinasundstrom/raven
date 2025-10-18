using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class PointerTypeSemanticTests : CompilationTestBase
{
    [Fact]
    public void PointerTypeSyntax_BindsToPointerTypeSymbol()
    {
        const string source = """
let value = 0
let pointer: *int = &value
""";
        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(d => d.Identifier.Text == "pointer");

        var typeInfo = model.GetTypeInfo(declarator.TypeAnnotation!.Type);
        var pointerType = Assert.IsAssignableFrom<IPointerTypeSymbol>(typeInfo.Type);
        Assert.Equal(SpecialType.System_Int32, pointerType.PointedAtType.SpecialType);
    }
}
