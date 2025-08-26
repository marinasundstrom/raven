using System.Linq;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class TupleTypeSemanticTests
{
    [Fact]
    public void TupleTypeSyntax_BindsToTupleTypeSymbol()
    {
        var source = """
        let t: (int, string) = (1, "")
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var type = model.GetTypeInfo(declarator.TypeAnnotation!.Type).Type;

        var tuple = Assert.IsAssignableFrom<ITupleTypeSymbol>(type);
        Assert.Collection(tuple.TupleElements,
            e => Assert.Equal(SpecialType.System_Int32, e.Type.SpecialType),
            e => Assert.Equal(SpecialType.System_String, e.Type.SpecialType));
    }
}
