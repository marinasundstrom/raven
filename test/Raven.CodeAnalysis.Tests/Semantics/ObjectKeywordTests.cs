using System.Linq;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ObjectKeywordTests
{
    [Fact]
    public void ObjectKeyword_BindsToSystemObject()
    {
        var source = """
        let o: object = null
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var type = model.GetTypeInfo(declarator.TypeAnnotation!.Type).Type!;

        Assert.Equal(SpecialType.System_Object, type.SpecialType);
    }
}
