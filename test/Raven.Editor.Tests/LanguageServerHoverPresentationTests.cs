using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.LanguageServer;

namespace Raven.Editor.Tests;

public class LanguageServerHoverPresentationTests
{
    [Fact]
    public void PromotedPrimaryConstructorParameter_ShowsPropertyKindAndContainingType()
    {
        const string code = """
class Foo(private var name: string) {
    func Test() -> string => name
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var targetFramework = TargetFrameworkResolver.ResolveLatestInstalledVersion();
        var references = TargetFrameworkResolver
            .GetReferenceAssemblies(targetFramework)
            .Select(MetadataReference.CreateFromFile);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in references)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var identifier = root.DescendantNodes().OfType<IdentifierNameSyntax>().Single(id => id.Identifier.ValueText == "name");
        var symbol = semanticModel.GetSymbolInfo(identifier).Symbol.ShouldBeAssignableTo<IParameterSymbol>();

        var buildKindDisplay = typeof(HoverHandler)
            .GetMethod("BuildKindDisplay", BindingFlags.NonPublic | BindingFlags.Static)!;
        var buildContainingDisplay = typeof(HoverHandler)
            .GetMethod("BuildContainingDisplay", BindingFlags.NonPublic | BindingFlags.Static)!;

        var kind = (string)buildKindDisplay.Invoke(null, [symbol])!;
        var containing = (string?)buildContainingDisplay.Invoke(null, [symbol, semanticModel]);

        kind.ShouldBe("Property");
        containing.ShouldBe("class Foo");
    }

    [Fact]
    public void DelegateTypeHover_UsesRavenFunctionTypeSignature()
    {
        const string code = """
class Functions {
    func Apply(value: int, transform: func (int) -> int) -> int {
        transform(value)
    }

    func Test() -> int {
        val increment: func (int) -> int = x => x + 1
        increment(1)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code, path: "/workspace/test.rav");
        var targetFramework = TargetFrameworkResolver.ResolveLatestInstalledVersion();
        var references = TargetFrameworkResolver
            .GetReferenceAssemblies(targetFramework)
            .Select(MetadataReference.CreateFromFile);

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree);

        foreach (var reference in references)
            compilation = compilation.AddReferences(reference);

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var functionTypeSyntax = root.DescendantNodes().OfType<FunctionTypeSyntax>().First();
        var localDeclarator = root.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(d => d.Identifier.Text == "increment");
        var localSymbol = semanticModel.GetDeclaredSymbol(localDeclarator).ShouldBeAssignableTo<ILocalSymbol>();
        var delegateType = localSymbol.Type.ShouldBeAssignableTo<INamedTypeSymbol>();

        var buildSignature = typeof(HoverHandler)
            .GetMethod("BuildSignature", BindingFlags.NonPublic | BindingFlags.Static)!;

        var signature = (string)buildSignature.Invoke(null, [delegateType, functionTypeSyntax, semanticModel])!;
        signature.ShouldStartWith("func (");
        signature.ShouldContain("->");
        signature.ShouldNotContain("Func(");
    }
}
