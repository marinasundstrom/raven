using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class IntersectionConstraintTests : CompilationTestBase
{
    [Theory]
    [InlineData("class Box<T: A & B> {}")]
    [InlineData("class Box<T> where T: A & B {}")]
    [InlineData("class Box<T> where T: (A & B) {}")]
    [InlineData("class Box<T> where T: A & (B & A) {}")]
    public void InterfaceBounds_AreExposedIndividually(string declaration)
    {
        var (compilation, tree) = CreateCompilation("interface A {}\ninterface B {}\n" + declaration);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var type = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax));
        var parameter = Assert.Single(type.TypeParameters);
        Assert.Equal(new[] { "A", "B" }, parameter.ConstraintTypes.Select(t => t.Name).Distinct());
    }

    [Theory]
    [InlineData("A", "B", "A & B")]
    [InlineData("B", "A", "A & B")]
    [InlineData("A", "B", "A, B")]
    [InlineData("B", "A", "A, B")]
    public void MissingConstituent_RejectsTypeArgument(string implemented, string missing, string bounds)
    {
        var (compilation, _) = CreateCompilation($$"""
            interface A {}
            interface B {}
            class Partial: {{implemented}} {}
            class Box<T: {{bounds}}> {}
            func Use(value: Box<Partial>) {}
            """);
        Assert.Contains(compilation.GetDiagnostics(), d =>
            d.Descriptor == CompilerDiagnostics.TypeArgumentDoesNotSatisfyConstraint &&
            d.GetMessage().Contains(missing));
    }

    [Theory]
    [InlineData("A & B & C")]
    [InlineData("int & A")]
    [InlineData("(A | D) & D")]
    [InlineData("T & A")]
    public void UnsupportedBounds_ReportDiagnostic(string bounds)
    {
        var (compilation, _) = CreateCompilation($$"""
            interface A {}
            interface D {}
            class B {}
            class C {}
            class Box<T: {{bounds}}> {}
            """);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Descriptor == CompilerDiagnostics.InvalidIntersectionConstraint);
    }

    [Fact]
    public void InaccessibleConstituent_ReportsItsOwnLocation()
    {
        var (compilation, tree) = CreateCompilation("""
            public interface Visible {}
            internal interface Hidden {}
            public class Box<T: Visible & Hidden> {}
            """);
        var diagnostic = Assert.Single(compilation.GetDiagnostics(), d => d.Id == "RAV0501");
        var bound = tree.GetRoot().DescendantNodes().OfType<IntersectionTypeSyntax>().Single().Types[1];
        Assert.Equal(bound.Span, diagnostic.Location.SourceSpan);
    }

    [Fact]
    public void ClassBoundWithStructConstraint_IsRejected()
    {
        var (compilation, _) = CreateCompilation("interface A {}\nclass B {}\nclass Box<T: struct, A & B> {}");
        Assert.Contains(compilation.GetDiagnostics(), d => d.Descriptor == CompilerDiagnostics.InvalidIntersectionConstraint);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void BothMembers_AreAvailableThroughPublicSemanticApi(bool diagnosticsFirst)
    {
        var (compilation, tree) = CreateCompilation("""
            interface A { func First() -> int }
            interface B { func Second() -> int }
            func Sum<T>(value: T) -> int where T: A & B {
                return value.First() + value.Second()
            }
            """);
        if (diagnosticsFirst)
            Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var calls = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().ToArray();
        Assert.Equal(new[] { "First", "Second" }, calls.Select(call => model.GetSymbolInfo(call).Symbol?.Name));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }
}
