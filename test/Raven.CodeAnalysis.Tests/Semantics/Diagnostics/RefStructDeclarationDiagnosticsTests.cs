using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class RefStructDeclarationDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void RefStructDeclaration_ProducesRefLikeSourceSymbol()
    {
        const string code = "ref struct Buffer<T> {}";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "test",
                [tree],
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);
        var declaration = tree.GetRoot().DescendantNodes().OfType<StructDeclarationSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));

        Assert.True(symbol.IsRefLikeType);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void RefModifier_IsRejectedOnClass()
    {
        const string code = "ref class Buffer {}";

        CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.ModifierNotValidOnMember.Id)
                .WithAnySpan()
                .WithArguments("ref", "class", "Buffer"),
        ]).Verify();
    }

    [Fact]
    public void RefModifier_CannotBeDuplicated()
    {
        const string code = "ref ref struct Buffer {}";

        CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.DuplicateModifier.Id)
                .WithAnySpan()
                .WithArguments("ref"),
        ]).Verify();
    }

    [Fact]
    public void PartialRefStructDeclarations_MustAgreeOnRefModifier()
    {
        const string code = """
            partial ref struct Buffer {}
            partial struct Buffer {}
            """;

        CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.PartialTypeDeclarationRefModifierMismatch.Id)
                .WithAnySpan()
                .WithArguments("Buffer"),
        ]).Verify();
    }
}
