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

        Assert.Equal("const MaxValue: char = 'z'", display);
        Assert.True(field.IsStatic);
        Assert.True(field.IsConst);
    }

    [Fact]
    public void Property_ToDisplayString_IncludesAccessorAccessibility()
    {
        const string source = """
class C {
    val Value: int { get; private set; }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var property = tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<IPropertySymbol>(model.GetDeclaredSymbol(property));

        var format = SymbolDisplayFormat.RavenCodeGenerationFormat
            .WithPropertyStyle(SymbolDisplayPropertyStyle.ShowReadWriteDescriptor);
        var display = symbol.ToDisplayString(format);

        Assert.Equal("Value: int { get; private set; }", display);
    }

    [Fact]
    public void Method_ToDisplayString_FormatsProtectedAccessibility()
    {
        const string source = """
class Base {
    protected func Run() -> unit { }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(method));

        var display = symbol.ToDisplayString(SymbolDisplayFormat.RavenCodeGenerationFormat);

        Assert.Equal("protected Run() -> ()", display);
    }

    [Fact]
    public void Method_ToDisplayString_FormatsAllAccessibilityModifiers()
    {
        const string source = """
class Base {
    private func PrivateRun() -> unit { }
    internal func InternalRun() -> unit { }
    protected func ProtectedRun() -> unit { }
    protected internal func ProtectedInternalRun() -> unit { }
    private protected func PrivateProtectedRun() -> unit { }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var methods = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .ToDictionary(
                static declaration => declaration.Identifier.ValueText,
                declaration => Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(declaration)));

        methods["PrivateRun"].ToDisplayString(SymbolDisplayFormat.RavenCodeGenerationFormat)
            .ShouldBe("private PrivateRun() -> ()");
        methods["InternalRun"].ToDisplayString(SymbolDisplayFormat.RavenCodeGenerationFormat)
            .ShouldBe("internal InternalRun() -> ()");
        methods["ProtectedRun"].ToDisplayString(SymbolDisplayFormat.RavenCodeGenerationFormat)
            .ShouldBe("protected ProtectedRun() -> ()");
        methods["ProtectedInternalRun"].ToDisplayString(SymbolDisplayFormat.RavenCodeGenerationFormat)
            .ShouldBe("protected internal ProtectedInternalRun() -> ()");
        methods["PrivateProtectedRun"].ToDisplayString(SymbolDisplayFormat.RavenCodeGenerationFormat)
            .ShouldBe("private protected PrivateProtectedRun() -> ()");
    }
}
