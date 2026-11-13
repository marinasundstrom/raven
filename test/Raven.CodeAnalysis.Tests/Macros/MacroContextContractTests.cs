using System.Collections.Immutable;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "Shared")]
public sealed class MacroContextContractTests
{
    [Theory]
    [InlineData("", "value")]
    [InlineData("a-b", "a_b")]
    [InlineData("$item", "item")]
    [InlineData("123", "23")]
    public void UniqueNamesNormalizeHintsAndAdvance(string hint, string normalized)
    {
        var context = new Context(SyntaxFactory.ParseExpression("0"));
        Assert.Equal($"__macro_{normalized}_0", context.CreateUniqueName(hint));
        Assert.Equal($"__macro_{normalized}_1", context.CreateUniqueIdentifier(hint).Identifier.ValueText);
    }

    [Fact]
    public void UniqueNamesScanTheEntireInvocationDocument()
    {
        var tree = SyntaxTree.ParseText("let __macro_item_0 = 1\nlet __macro_item_1 = 2\nlet x = 3");
        var invocation = tree.GetRoot().DescendantNodes().OfType<LiteralExpressionSyntax>().Last();
        var context = new Context(invocation);
        Assert.Equal("__macro_item_2", context.CreateUniqueName("item"));
        Assert.Equal("__macro_item_3", context.CreateUniqueName("item"));
        Assert.Equal("__macro_other_0", context.CreateUniqueName("other"));
    }

    [Fact]
    public void RequiredShapeReturnsIdentityOrReportsAtAuthoredOrigin()
    {
        var source = SyntaxTree.ParseText(new string(' ', 40), path: "macro-origin.rvn");
        var invocation = source.GetRoot();
        var context = new Context(invocation);
        var expression = SyntaxFactory.ParseExpression("1+2");
        Assert.Same(expression, context.RequireSyntax<InfixOperatorExpressionSyntax>(expression));
        var mapped = MacroSyntaxOrigin.AttachBodyOrigin(expression, source, new TextSpan(10, 20), new TextSpan(2, 3));
        Assert.True(MacroSyntaxOrigin.TryGetSourceSpan(mapped, source, out var mappedSpan));
        Assert.Equal(new TextSpan(12, 3), mappedSpan);
        Assert.Same(source, context.InvocationSyntax.SyntaxTree);
        Assert.Null(context.RequireSyntax<IdentifierNameSyntax>(mapped, "name required", "M001"));
        var diagnostic = Assert.Single(context.GetReportedMacroDiagnostics());
        Assert.Equal(new TextSpan(12, 3), diagnostic.Location!.SourceSpan);
        Assert.Equal("name required", diagnostic.Message);
        Assert.Equal("M001", diagnostic.Code);
    }

    [Fact]
    public void DetachedMismatchUsesInvocationAndDefaultMessage()
    {
        var invocation = SyntaxTree.ParseText("call()", path: "invocation.rvn").GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var context = new Context(invocation);
        Assert.Null(context.RequireSyntax<IdentifierNameSyntax>(SyntaxFactory.ParseExpression("1+2")));
        var diagnostic = Assert.Single(context.GetReportedMacroDiagnostics());
        Assert.Equal(invocation.Span, diagnostic.Location!.SourceSpan);
        Assert.Contains("Expected IdentifierNameSyntax", diagnostic.Message);
    }

    [Fact]
    public void DiagnosticCollectionsSnapshotAndMergeWithoutLosingOrder()
    {
        var context = new Context(SyntaxFactory.ParseExpression("1"));
        var other = new Context(SyntaxFactory.ParseExpression("2"));
        var descriptor = DiagnosticDescriptor.Create("TEST", "title", null, "", "message", "test", DiagnosticSeverity.Warning);
        var first = Diagnostic.Create(descriptor, Location.None);
        context.ReportDiagnostic(first);
        var snapshot = context.GetReportedDiagnostics();
        var second = Diagnostic.Create(descriptor, Location.None);
        other.ReportDiagnostics(new[] { second });
        var macro = MacroExpansionDiagnostic.Error("macro");
        other.ReportDiagnostics(new[] { macro });
        context.AddReportedDiagnostics(other);
        Assert.Same(first, Assert.Single(snapshot));
        Assert.Equal(new[] { first, second }, context.GetReportedDiagnostics());
        Assert.Same(macro, Assert.Single(context.GetReportedMacroDiagnostics()));
        var syntax = SyntaxFactory.ParseExpression("1");
        context.ReportDiagnostics(new MacroSyntaxParseResult<ExpressionSyntax>(syntax, default, 1, [second]));
        Assert.Equal(3, context.GetReportedDiagnostics().Length);
    }

    [Fact]
    public void ParseResultNormalizesDefaultDiagnosticsAndPreservesValueFields()
    {
        var syntax = SyntaxFactory.ParseExpression("1");
        var result = new MacroSyntaxParseResult<ExpressionSyntax>(syntax, new TextSpan(3, 1), 4, default);
        var equal = new MacroSyntaxParseResult<ExpressionSyntax>(syntax, new TextSpan(3, 1), 4, []);
        Assert.True(result == equal);
        Assert.Same(syntax, result.Syntax);
        Assert.Equal(new TextSpan(3, 1), result.BodyRelativeSpan);
        Assert.Equal(4, result.ConsumedBodyRelativeEnd);
        Assert.Empty(result.Diagnostics);
        Assert.False(result.HasErrors);
        Assert.True(result != new MacroSyntaxParseResult<ExpressionSyntax>(syntax, new TextSpan(4, 1), 5, []));
    }

    [Fact]
    public void NullInputsRetainArgumentValidation()
    {
        Assert.Throws<ArgumentNullException>(() => new Context(null!));
        var context = new Context(SyntaxFactory.ParseExpression("1"));
        Assert.Throws<ArgumentNullException>(() => context.CreateUniqueName(null!));
        Assert.Throws<ArgumentNullException>(() => context.RequireSyntax<ExpressionSyntax>(null!));
        Assert.Throws<ArgumentNullException>(() => context.ReportDiagnostic((Diagnostic)null!));
        Assert.Throws<ArgumentNullException>(() => context.ReportDiagnostic((MacroExpansionDiagnostic)null!));
        Assert.Throws<ArgumentNullException>(() => context.AddReportedDiagnostics(null!));
    }

    [Fact]
    public void ParsedOriginsResolveThroughSourceServicesAndRejectOtherDocuments()
    {
        var source = SyntaxTree.ParseText("1+2", path: "authored.rvn");
        var expression = source.GetRoot().DescendantNodes().OfType<InfixOperatorExpressionSyntax>().Single();
        var mapped = MacroSyntaxOrigin.AttachParsedOrigin(expression, source);
        Assert.True(MacroSyntaxOrigin.ContainsAuthoredOrigin(mapped));
        Assert.True(MacroSyntaxOrigin.TryGetSourceSpan(mapped, source, out var span));
        Assert.Equal(expression.Span, span);
        var other = SyntaxTree.ParseText("1+2", path: "other.rvn");
        Assert.False(MacroSyntaxOrigin.TryGetSourceSpan(mapped, other, out _));
        var services = Symbols.MetadataSymbolFixture.Make<SourceDeclarationServices>(SymbolKind.Assembly, "Services");
        Symbols.MetadataSymbolFixture.Set(services, "get_SyntaxTrees", new[] { source });
        Assert.True(MacroSyntaxOrigin.TryGetFirstAuthoredSourceSpan(mapped, services, out var actualTree, out span));
        Assert.Same(source, actualTree);
        Assert.Equal(expression.Span, span);
        Assert.Same(expression, MacroSyntaxOrigin.AttachParsedOrigin(expression, null));
    }

    [Fact]
    public void NarrowestSourceMapWinsAndHiddenSyntaxRespectsAuthoredNodes()
    {
        var source = SyntaxTree.ParseText(new string(' ', 100), path: "mapped.rvn");
        var expression = SyntaxFactory.ParseExpression("1+2");
        var mapped = MacroSyntaxOrigin.AttachMappedOrigins(expression, source, new TextSpan(10, 80),
            new[] { new MacroExpansionSourceMap(new TextSpan(0, 20), new TextSpan(0, 20)),
                new MacroExpansionSourceMap(new TextSpan(0, 3), new TextSpan(5, 3)) });
        Assert.True(MacroSyntaxOrigin.TryGetSourceSpan(mapped, source, out var span));
        Assert.Equal(new TextSpan(15, 3), span);
        Assert.False(MacroSyntaxOrigin.IsHidden(MacroSyntaxOrigin.MarkGeneratedSyntaxHidden(expression, expression)));
        Assert.True(MacroSyntaxOrigin.IsHidden(MacroSyntaxOrigin.MarkGeneratedSyntaxHidden(SyntaxFactory.ParseExpression("4+5"), expression)));
        Assert.False(MacroSyntaxOrigin.IsHidden(MacroSyntaxOrigin.MarkGeneratedSyntaxHidden(mapped, expression)));
    }

    private sealed class Context(SyntaxNode invocation) : MacroContext(invocation)
    {
        public override MacroExpansionDiagnostic CreateDiagnostic(string message,
            DiagnosticSeverity severity = DiagnosticSeverity.Error, SyntaxNode? syntax = null, string? code = null)
            => new(severity, message, (syntax ?? InvocationSyntax).GetLocation(), code);
    }
}
