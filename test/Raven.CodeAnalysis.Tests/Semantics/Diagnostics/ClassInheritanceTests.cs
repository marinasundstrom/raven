using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ClassInheritanceTests : CompilationTestBase
{
    [Fact]
    public void SealedBaseClass_DerivationProducesDiagnostic()
    {
        var source = """
class Parent {};
class Derived : Parent {};
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal("RAV0306", diagnostic.Descriptor.Id);
    }

    [Fact]
    public void AbstractBaseClass_DerivationWithoutOpen_Succeeds()
    {
        const string source = """
abstract class Animal {}

class Dog : Animal {}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var abstractDeclaration = (ClassDeclarationSyntax)root.Members[0];
        var symbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(abstractDeclaration));

        Assert.True(symbol.IsAbstract);
        Assert.False(symbol.IsClosed);
    }

    [Fact]
    public void StaticBaseClass_DerivationProducesDiagnostic()
    {
        var source = """
static class Parent {}

class Derived : Parent {}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal("RAV0328", diagnostic.Descriptor.Id);
    }
}
