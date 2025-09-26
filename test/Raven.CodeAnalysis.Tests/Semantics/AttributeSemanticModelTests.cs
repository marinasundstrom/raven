using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class AttributeSemanticModelTests : CompilationTestBase
{
    [Fact]
    public void GetAttribute_ReturnsConstructorAndArguments()
    {
        const string source = """
class InfoAttribute : System.Attribute
{
    public init(name: string, kind: System.Type) {}
}

[Info(name: "Widget", kind: typeof(int))]
class Widget {}
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var attribute = root.DescendantNodes().OfType<AttributeSyntax>().Single();
        var widgetDeclaration = root.DescendantNodes().OfType<ClassDeclarationSyntax>().Single(c => c.Identifier.Text == "Widget");

        var symbolInfo = model.GetSymbolInfo(attribute);
        var constructor = Assert.IsAssignableFrom<IMethodSymbol>(symbolInfo.Symbol);
        var containingType = Assert.IsAssignableFrom<INamedTypeSymbol>(constructor.ContainingType);
        Assert.Equal("InfoAttribute", containingType.Name);

        var widgetSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(widgetDeclaration));
        var data = Assert.Single(widgetSymbol.GetAttributes());
        Assert.Same(constructor, data.AttributeConstructor);
        Assert.Equal("InfoAttribute", data.AttributeClass.Name);
        Assert.Equal(2, data.ConstructorArguments.Length);

        var nameArgument = data.ConstructorArguments[0];
        Assert.Equal(TypedConstantKind.Primitive, nameArgument.Kind);
        Assert.Equal("Widget", nameArgument.Value);

        var typeArgument = data.ConstructorArguments[1];
        Assert.Equal(TypedConstantKind.Type, typeArgument.Kind);
        var typeSymbol = Assert.IsAssignableFrom<ITypeSymbol>(typeArgument.Value);
        Assert.Equal("System.Int32", typeSymbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
    }
}
