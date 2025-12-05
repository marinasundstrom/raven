using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class ExpressionBodiedPropertyBindingTests
{
    [Fact]
    public void ExpressionBodiedProperty_BindsToPropertySymbol()
    {
        var syntaxTree = SyntaxTree.ParseText(
            """
            class C {
                public IsOk: int => 2
            }
            """);

        var compilation = Compilation.Create(
            "test",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(syntaxTree);
        var propertySyntax = syntaxTree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();

        var symbol = Assert.IsAssignableFrom<IPropertySymbol>(model.GetDeclaredSymbol(propertySyntax));
        Assert.Equal("IsOk", symbol.Name);
    }
}
