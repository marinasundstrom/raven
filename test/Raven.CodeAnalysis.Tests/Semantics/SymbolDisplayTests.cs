using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class SymbolDisplayTests : CompilationTestBase
{
    [Fact]
    public void ConstField_ToDisplayString_ExcludesStaticModifier()
    {
        const string source = """
class C {
    public const MaxValue: char = 'z'
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var field = Assert.IsAssignableFrom<IFieldSymbol>(model.GetDeclaredSymbol(declarator));

        var display = field.ToDisplayString(SymbolDisplayFormat.RavenCodeGenerationFormat);

        Assert.Equal("public const MaxValue: char", display);
        Assert.True(field.IsStatic);
        Assert.True(field.IsConst);
    }

    [Fact]
    public void Property_ToDisplayString_IncludesAccessorAccessibility()
    {
        const string source = """
class C {
    public Value: int { get; private set; }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var property = tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<IPropertySymbol>(model.GetDeclaredSymbol(property));

        var format = SymbolDisplayFormat.RavenCodeGenerationFormat
            .WithPropertyStyle(SymbolDisplayPropertyStyle.ShowReadWriteDescriptor);
        var display = symbol.ToDisplayString(format);

        Assert.Equal("public Value: int { get; private set; }", display);
    }
}
