using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class QualifiedNameBindingTests : CompilationTestBase
{
    [Fact]
    public void QualifiedName_PrefersTypeOverNamespace()
    {
        const string source = """
using Lib;

namespace Lib
{
    class Container
    {
        public class Inner {}
    }
}

namespace Container
{
    class Inner {}
}

class Holder
{
    public Value: Container.Inner { get; }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var property = root.DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();
        var propertySymbol = Assert.IsAssignableFrom<IPropertySymbol>(model.GetDeclaredSymbol(property));

        Assert.Equal(
            "Lib.Container.Inner",
            propertySymbol.Type.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));

        Assert.Empty(compilation.GetDiagnostics());
    }
}
