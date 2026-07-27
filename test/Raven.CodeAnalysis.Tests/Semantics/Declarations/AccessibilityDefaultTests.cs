using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests.Declarations;

public sealed class AccessibilityDefaultTests : CompilationTestBase
{
    [Fact]
    public void NamespaceLevelTypes_DefaultToInternal()
    {
        const string source = """
class C {}
struct S {}
interface I {}
enum E { A }
delegate D()
union U { case A }
extension X for int {}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var symbols = tree.GetRoot().Members
            .Select(model.GetDeclaredSymbol)
            .OfType<INamedTypeSymbol>()
            .ToArray();

        Assert.Equal(7, symbols.Length);
        Assert.All(symbols, symbol => Assert.Equal(Accessibility.Internal, symbol.DeclaredAccessibility));
    }

    [Fact]
    public void ExplicitPublicExportsNamespaceLevelTypes()
    {
        const string source = """
public class C {}
public struct S {}
public interface I {}
public enum E { A }
public delegate D()
public union U { case A }
public extension X for int {}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var symbols = tree.GetRoot().Members
            .Select(model.GetDeclaredSymbol)
            .OfType<INamedTypeSymbol>()
            .ToArray();

        Assert.Equal(7, symbols.Length);
        Assert.All(symbols, symbol => Assert.Equal(Accessibility.Public, symbol.DeclaredAccessibility));
        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Id == CompilerDiagnostics.PublicModifierRedundant.Id);
    }

    [Fact]
    public void TypeMembers_DefaultToPublic()
    {
        const string source = """
class Container {
    class Nested {}
    func Run() {}
    var Value: int { get; set; }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var containerDeclaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().First();
        var container = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(containerDeclaration));

        Assert.Equal(Accessibility.Internal, container.DeclaredAccessibility);
        Assert.Equal(Accessibility.Public, Assert.Single(container.GetTypeMembers("Nested")).DeclaredAccessibility);
        Assert.Equal(Accessibility.Public, Assert.Single(container.GetMembers("Run").OfType<IMethodSymbol>()).DeclaredAccessibility);
        Assert.Equal(Accessibility.Public, Assert.Single(container.GetMembers("Value").OfType<IPropertySymbol>()).DeclaredAccessibility);
    }

    [Fact]
    public void TypeMembers_CanExplicitlyRestrictAccessibility()
    {
        const string source = """
public class Api {
    internal func Prepare() {}
    protected func Extend() {}
    private func Reset() {}
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var api = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(declaration));

        Assert.Equal(Accessibility.Internal, Assert.Single(api.GetMembers("Prepare").OfType<IMethodSymbol>()).DeclaredAccessibility);
        Assert.Equal(Accessibility.ProtectedAndProtected, Assert.Single(api.GetMembers("Extend").OfType<IMethodSymbol>()).DeclaredAccessibility);
        Assert.Equal(Accessibility.Private, Assert.Single(api.GetMembers("Reset").OfType<IMethodSymbol>()).DeclaredAccessibility);
    }

}
