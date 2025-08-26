using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Tests;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class MethodOverloadTests
{
    [Fact]
    public void Overloads_DifferOnlyByNullableReferenceType_AreRejected()
    {
        var source = """
        class C {
            f(x: string) -> int { 0 }
            f(x: string?) -> int { 1 }
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var methods = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().ToArray();
        _ = model.GetDeclaredSymbol(methods[0]);
        _ = model.GetDeclaredSymbol(methods[1]);

        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.TypeAlreadyDefinesMember, diagnostic.Descriptor);
    }

    [Fact]
    public void Overloads_WithNullableValueType_AreAllowed()
    {
        var source = """
        class C {
            f(x: int) -> int { 0 }
            f(x: int?) -> int { 1 }
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var methods = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().ToArray();
        _ = model.GetDeclaredSymbol(methods[0]);
        _ = model.GetDeclaredSymbol(methods[1]);

        Assert.Empty(compilation.GetDiagnostics());
    }
}
