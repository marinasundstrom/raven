using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class IntersectionReceiverBindingTests : CompilationTestBase
{
    [Theory]
    [InlineData(false, "int")]
    [InlineData(true, "int")]
    [InlineData(false, "string")]
    [InlineData(true, "string")]
    public void PropertyConflict_ReturnsAmbiguousCandidates(bool reverse, string secondType)
    {
        var (compilation, _) = CreateCompilation($$"""
            interface A { val Value: int }
            interface B { val Value: {{secondType}} }
            """);
        AssertNoErrors(compilation);
        var binder = CreateBinder(compilation, reverse);
        var syntax = ParseExpression("value.Value");
        var bound = Assert.IsType<BoundErrorExpression>(binder.BindExpression(syntax));
        Assert.Equal(BoundExpressionReason.Ambiguous, bound.Reason);
        Assert.Equal(new[] { "A", "B" }, bound.Candidates.Select(c => c.ContainingType!.Name).OrderBy(n => n));
        var diagnostic = Assert.Single(binder.Diagnostics.AsEnumerable(), d => d.Id == "RAV0365");
        Assert.Equal(((MemberAccessExpressionSyntax)syntax).Name.Span, diagnostic.Location.SourceSpan);
        Assert.Contains("A.Value", diagnostic.GetMessage());
        Assert.Contains("B.Value", diagnostic.GetMessage());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void PropertyAssignment_DoesNotSelectFirstConstituent(bool reverse)
    {
        var (compilation, _) = CreateCompilation("""
            interface A { var Value: int }
            interface B { var Value: int }
            """);
        AssertNoErrors(compilation);
        var binder = CreateBinder(compilation, reverse);
        var statement = SyntaxTree.ParseText("value.Value = 1").GetRoot().DescendantNodes().OfType<AssignmentStatementSyntax>().Single();
        _ = binder.BindStatement(statement);
        Assert.Contains(binder.Diagnostics.AsEnumerable(), d => d.Id == "RAV0365");
    }

    [Fact]
    public void SharedInheritedProperty_BindsOneDeclaration()
    {
        var (compilation, _) = CreateCompilation("""
            interface Root { val Value: int }
            interface A: Root {}
            interface B: Root {}
            """);
        AssertNoErrors(compilation);
        var binder = CreateBinder(compilation);
        var bound = Assert.IsType<BoundMemberAccessExpression>(binder.BindExpression(ParseExpression("value.Value")));
        Assert.Equal("Root", bound.Symbol?.ContainingType?.Name);
        Assert.DoesNotContain(binder.Diagnostics.AsEnumerable(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void InaccessibleProperty_DoesNotMakeAccessiblePropertyAmbiguous(bool reverse)
    {
        var (compilation, _) = CreateCompilation("""
            class A { private val Value: int => 1 }
            interface B { val Value: int }
            """);
        AssertNoErrors(compilation);
        var binder = CreateBinder(compilation, reverse);
        var bound = Assert.IsType<BoundMemberAccessExpression>(binder.BindExpression(ParseExpression("value.Value")));
        Assert.Equal("B", bound.Symbol?.ContainingType?.Name);
        Assert.DoesNotContain(binder.Diagnostics.AsEnumerable(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void PropertyConflict_PreservesAllCandidates()
    {
        var (compilation, _) = CreateCompilation("""
            interface A { val Value: int }
            interface B { val Value: int }
            interface C { val Value: int }
            """);
        AssertNoErrors(compilation);
        var type = compilation.CreateIntersectionTypeSymbol(
            compilation.GetTypeByMetadataName("A")!,
            compilation.GetTypeByMetadataName("B")!,
            compilation.GetTypeByMetadataName("C")!);
        var binder = new ReceiverBinder(compilation, type);
        var bound = Assert.IsType<BoundErrorExpression>(binder.BindExpression(ParseExpression("value.Value")));
        Assert.Equal(BoundExpressionReason.Ambiguous, bound.Reason);
        Assert.Equal(new[] { "A", "B", "C" }, bound.Candidates.Select(c => c.ContainingType!.Name).OrderBy(n => n));
        Assert.Single(binder.Diagnostics.AsEnumerable(), d => d.Id == "RAV0365");
    }

    private static ReceiverBinder CreateBinder(Compilation compilation, bool reverse = false)
    {
        var a = compilation.GetTypeByMetadataName("A")!;
        var b = compilation.GetTypeByMetadataName("B")!;
        var type = reverse ? compilation.CreateIntersectionTypeSymbol(b, a) : compilation.CreateIntersectionTypeSymbol(a, b);
        return new ReceiverBinder(compilation, type);
    }

    private static ExpressionSyntax ParseExpression(string text)
        => SyntaxTree.ParseText(text).GetRoot().DescendantNodes().OfType<ExpressionStatementSyntax>().Single().Expression;

    private static void AssertNoErrors(Compilation compilation)
        => Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);

    // Exercise normal source-expression binding without enabling an unsupported
    // source annotation or installing a runtime representation for the local.
    private sealed class ReceiverBinder : BlockBinder
    {
        private readonly ILocalSymbol _receiver;
        public override SemanticModel SemanticModel { get; }

        public ReceiverBinder(Compilation compilation, ITypeSymbol type)
            : base(compilation.Assembly, compilation.GlobalBinder)
        {
            SemanticModel = compilation.GetSemanticModel(compilation.SyntaxTrees.First());
            _receiver = new SourceLocalSymbol("value", type, false, compilation.Assembly, null, null, [], []);
        }

        public override IEnumerable<ISymbol> LookupSymbols(string name)
            => name == "value" ? [_receiver] : base.LookupSymbols(name);
    }
}
