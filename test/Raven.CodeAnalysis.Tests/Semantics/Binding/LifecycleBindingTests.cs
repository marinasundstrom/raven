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

        var (compilation, tree) = CreateCompilation(
            source,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithAllowUnsafe(true));
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

        var (compilation, tree) = CreateCompilation(
            source,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithAllowUnsafe(true));
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));

        var model = compilation.GetSemanticModel(tree);
        var initDecl = tree.GetRoot().DescendantNodes().OfType<InitDeclarationSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(initDecl));

        Assert.Equal(MethodKind.StaticConstructor, symbol.MethodKind);
        Assert.Equal(".cctor", symbol.Name);
        Assert.True(symbol.IsStatic);
    }

    [Fact]
    public void FinallyDeclaration_BindsAsDestructorMethodKind()
    {
        const string source = """
class C {
    finally {
    }
}
""";

        var (compilation, tree) = CreateCompilation(
            source,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithAllowUnsafe(true));
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));

        var model = compilation.GetSemanticModel(tree);
        var finalDecl = tree.GetRoot().DescendantNodes().OfType<FinallyDeclarationSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(finalDecl));

        Assert.Equal(MethodKind.Destructor, symbol.MethodKind);
        Assert.Equal("Finalize", symbol.Name);
        Assert.False(symbol.IsStatic);
    }

    [Fact]
    public void UnsafeLifecycleDeclarations_BindAsUnsafeMethods()
    {
        const string source = """
class C {
    unsafe static init {
    }

    unsafe init {
    }

    unsafe finally {
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));

        var model = compilation.GetSemanticModel(tree);
        var initDecls = tree.GetRoot().DescendantNodes().OfType<InitDeclarationSyntax>().ToArray();
        var finalDecl = tree.GetRoot().DescendantNodes().OfType<FinallyDeclarationSyntax>().Single();

        Assert.Equal(2, initDecls.Length);

        var staticInitDecl = Assert.Single(initDecls.Where(d => d.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword)));
        var staticInitSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(staticInitDecl));
        Assert.Equal(MethodKind.StaticConstructor, staticInitSymbol.MethodKind);
        Assert.True(staticInitSymbol.IsUnsafe);

        var instanceInitDecl = Assert.Single(initDecls.Where(d => !d.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword)));
        var instanceInitSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(instanceInitDecl));
        Assert.Equal(MethodKind.Constructor, instanceInitSymbol.MethodKind);

        var finalSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(finalDecl));
        Assert.Equal(MethodKind.Destructor, finalSymbol.MethodKind);
        Assert.True(finalSymbol.IsUnsafe);
    }

    [Fact]
    public void MultipleFinallyDeclarations_ReportDuplicateMemberDiagnostic()
    {
        const string source = """
class C {
    finally { }
    finally { }
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();

        Assert.Contains(diagnostics, d => d.Id == CompilerDiagnostics.TypeAlreadyDefinesMember.Id);
    }
}
