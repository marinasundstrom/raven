using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class LifecycleBindingTests : CompilationTestBase
{
    [Fact]
    public void InitDeclaration_BindsAsConstructor()
    {
        const string source = """
class C {
    init {
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));

        var model = compilation.GetSemanticModel(tree);
        var initDecl = tree.GetRoot().DescendantNodes().OfType<InitDeclarationSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(initDecl));

        Assert.Equal(MethodKind.Constructor, symbol.MethodKind);
        Assert.Equal(".ctor", symbol.Name);
        Assert.Equal(0, symbol.Parameters.Length);
    }

    [Fact]
    public void StaticInitDeclaration_BindsAsStaticConstructor()
    {
        const string source = """
class C {
    static init {
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));

        var model = compilation.GetSemanticModel(tree);
        var initDecl = tree.GetRoot().DescendantNodes().OfType<InitDeclarationSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(initDecl));

        Assert.Equal(MethodKind.StaticConstructor, symbol.MethodKind);
        Assert.Equal(".cctor", symbol.Name);
        Assert.True(symbol.IsStatic);
    }

    [Fact]
    public void FinalDeclaration_BindsAsDestructorMethodKind()
    {
        const string source = """
class C {
    final {
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));

        var model = compilation.GetSemanticModel(tree);
        var finalDecl = tree.GetRoot().DescendantNodes().OfType<FinalDeclarationSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(finalDecl));

        Assert.Equal(MethodKind.Destructor, symbol.MethodKind);
        Assert.Equal("Finalize", symbol.Name);
        Assert.False(symbol.IsStatic);
    }
}
