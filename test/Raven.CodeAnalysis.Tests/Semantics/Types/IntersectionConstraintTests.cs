using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class IntersectionConstraintTests : CompilationTestBase
{
    [Theory]
    [InlineData("class Box<T: A & B> {}", false)]
    [InlineData("class Box<T: A & B> {}", true)]
    [InlineData("class Box<T> where T: (A & (B & A)) {}", false)]
    [InlineData("class Box<T> where T: (A & (B & A)) {}", true)]
    [InlineData("func Use<T>() where T: A & B {}", false)]
    [InlineData("func Use<T>() where T: A & B {}", true)]
    public void CompoundConstraint_PublicQueriesExposeIntersection(string declaration, bool diagnosticsFirst)
    {
        var (compilation, tree) = CreateCompilation("interface A {}\ninterface B {}\n" + declaration);
        if (diagnosticsFirst)
            Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);

        var model = compilation.GetSemanticModel(tree);
        var constraint = tree.GetRoot().DescendantNodes().OfType<TypeConstraintSyntax>().Single();
        var type = Assert.IsAssignableFrom<IIntersectionTypeSymbol>(model.GetTypeInfo(constraint.Type).Type);
        Assert.Equal(new[] { "A", "B" }, type.ConstituentTypes.Select(t => t.Name));
        Assert.True(SymbolEqualityComparer.Default.Equals(type, model.GetSymbolInfo(constraint.Type).Symbol));
        foreach (var syntax in constraint.DescendantNodes().OfType<IntersectionTypeSyntax>())
            Assert.IsAssignableFrom<IIntersectionTypeSymbol>(model.GetTypeInfo(syntax).Type);
        foreach (var name in constraint.DescendantNodes().OfType<IdentifierNameSyntax>())
            Assert.Equal(name.Identifier.ValueText, model.GetSymbolInfo(name).Symbol?.Name);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        Assert.True(SymbolEqualityComparer.Default.Equals(type, model.GetTypeInfo(constraint.Type).Type));
    }

    [Fact]
    public void CompoundConstraint_ResolvesGenericConstituentsInMethodScope()
    {
        var (compilation, tree) = CreateCompilation("""
            interface A<T> {}
            interface B<T> {}
            func Use<T, U>() where U: A<T> & B<T> {}
            """);
        var model = compilation.GetSemanticModel(tree);
        var syntax = tree.GetRoot().DescendantNodes().OfType<IntersectionTypeSyntax>().Single();
        var type = Assert.IsAssignableFrom<IIntersectionTypeSymbol>(model.GetTypeInfo(syntax).Type);
        foreach (var constituent in type.ConstituentTypes)
        {
            var named = Assert.IsAssignableFrom<INamedTypeSymbol>(constituent);
            var argument = Assert.IsAssignableFrom<ITypeParameterSymbol>(Assert.Single(named.TypeArguments));
            Assert.Equal("T", argument.Name);
            Assert.Equal("Use", argument.ContainingSymbol?.Name);
        }
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void CompoundConstraint_PublicQueryNormalizesNominalSupertypes()
    {
        var (compilation, tree) = CreateCompilation("interface A {}\ninterface B: A {}\nclass Box<T: A & B> {}");
        var syntax = tree.GetRoot().DescendantNodes().OfType<IntersectionTypeSyntax>().Single();
        Assert.Equal("B", compilation.GetSemanticModel(tree).GetTypeInfo(syntax).Type?.Name);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Theory]
    [InlineData("class Box<T: A & UnresolvedBound> {}")]
    [InlineData("class Box<T> where T: A, UnresolvedBound {}")]
    [InlineData("func Use<T>() where T: A & UnresolvedBound {}")]
    [InlineData("func Use<T>() where T: A, UnresolvedBound {}")]
    public void Constraint_MissingConstituentRemainsDiagnosedAfterTypeQuery(string declaration)
    {
        var (compilation, tree) = CreateCompilation("interface A {}\n" + declaration);
        var syntax = tree.GetRoot().DescendantNodes().OfType<TypeConstraintSyntax>()
            .Single(constraint => constraint.Type.ToString().Contains("UnresolvedBound")).Type;
        var type = compilation.GetSemanticModel(tree).GetTypeInfo(syntax).Type;
        Assert.True(type is null || type.TypeKind == TypeKind.Error, $"Unexpected type: {type?.ToDisplayString()} ({type?.TypeKind})");
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

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
