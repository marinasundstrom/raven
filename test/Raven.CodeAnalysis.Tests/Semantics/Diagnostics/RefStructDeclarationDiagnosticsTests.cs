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

    [Fact]
    public void ReadonlyModifier_IsRejectedOnClass()
    {
        const string code = "readonly class Buffer {}";

        CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.ModifierNotValidOnMember.Id)
                .WithAnySpan()
                .WithArguments("readonly", "class", "Buffer"),
        ]).Verify();
    }

    [Fact]
    public void ReadonlyRefStruct_ProducesReadonlySourceSymbol()
    {
        const string code = "readonly ref struct Buffer {}";

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
        Assert.True(symbol.IsReadOnly);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void ReadonlyStruct_CannotContainMutableField()
    {
        const string code = """
            readonly ref struct Buffer {
                field Value: int
            }
            """;

        CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.ReadonlyStructMemberMustBeReadonly.Id)
                .WithAnySpan()
                .WithArguments("Value"),
        ]).Verify();
    }

    [Fact]
    public void ReadonlyStruct_CannotContainMutableProperty()
    {
        const string code = """
            readonly ref struct Buffer {
                var Value: int
            }
            """;

        CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.ReadonlyStructMemberMustBeReadonly.Id)
                .WithAnySpan()
                .WithArguments("Value"),
        ]).Verify();
    }

    [Fact]
    public void PartialReadonlyStructDeclarations_MustAgreeOnReadonlyModifier()
    {
        const string code = """
            partial readonly ref struct Buffer {}
            partial ref struct Buffer {}
            """;

        CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.PartialTypeDeclarationReadonlyModifierMismatch.Id)
                .WithAnySpan()
                .WithArguments("Buffer"),
        ]).Verify();
    }
}
