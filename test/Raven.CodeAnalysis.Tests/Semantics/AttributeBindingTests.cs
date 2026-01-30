using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class AttributeBindingTests : CompilationTestBase
{
    [Fact]
    public void AttributeNameIsResolvedFromImports()
    {
        const string source = """
import System.*

[Obsolete(\"use new\")]
class C { }
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var classDeclaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var type = (INamedTypeSymbol)model.GetDeclaredSymbol(classDeclaration)!;

        var attribute = Assert.Single(type.GetAttributes());

        Assert.Equal("ObsoleteAttribute", attribute.AttributeClass?.Name);
        Assert.Equal("use new", attribute.ConstructorArguments.Single().Value);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void AttributeNameIsResolvedAfterTopLevelStatements()
    {
        const string source = """
import System.*
import System.Console.*

Console.WriteLine("Test");

[Obsolete("Hello")]
class Foo { }
""";

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var classDeclaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var type = (INamedTypeSymbol)model.GetDeclaredSymbol(classDeclaration)!;

        var attribute = Assert.Single(type.GetAttributes());

        Assert.Equal("ObsoleteAttribute", attribute.AttributeClass?.Name);
        Assert.Equal("Hello", attribute.ConstructorArguments.Single().Value);
        Assert.Empty(compilation.GetDiagnostics());
    }
}
