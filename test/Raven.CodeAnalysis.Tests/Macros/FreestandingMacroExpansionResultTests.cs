using System.Collections.Immutable;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Macros;

public sealed class FreestandingMacroExpansionResultTests
{
    [Fact]
    public void FromExpression_CreatesExpressionResultWithNormalizedDiagnostics()
    {
        var expression = SyntaxFactory.LiteralExpression(
            SyntaxKind.NumericLiteralExpression,
            SyntaxFactory.Literal(42));

        var result = FreestandingMacroExpansionResult.FromExpression(
            expression,
            default,
            default);

        result.Expression.ShouldBeSameAs(expression);
        result.Node.ShouldBeSameAs(expression);
        result.Diagnostics.ShouldBe(ImmutableArray<Diagnostic>.Empty);
        result.MacroDiagnostics.ShouldBe(ImmutableArray<MacroExpansionDiagnostic>.Empty);
    }

    [Fact]
    public void FromDiagnostic_CreatesDiagnosticOnlyResult()
    {
        var diagnostic = MacroExpansionDiagnostic.Error("Expansion failed.");

        var result = FreestandingMacroExpansionResult.FromDiagnostic(diagnostic);

        result.Expression.ShouldBeNull();
        result.Node.ShouldBeNull();
        result.Diagnostics.ShouldBeEmpty();
        result.MacroDiagnostics.Length.ShouldBe(1);
        result.MacroDiagnostics[0].ShouldBeSameAs(diagnostic);
    }

    [Fact]
    public void FromStatement_CreatesStatementResultWithTypedProjection()
    {
        var statement = SyntaxFactory.ParseStatement("return 42")!;

        var result = FreestandingMacroExpansionResult.FromStatement(statement);

        result.Node.ShouldBeSameAs(statement);
        result.Statement.ShouldBeSameAs(statement);
        result.Expression.ShouldBeNull();
    }

    [Fact]
    public void Builder_PreservesGeneralSyntaxNodeExpansion()
    {
        var statement = SyntaxFactory.ParseStatement("return 42")!;
        var builder = new MacroExpansionResultBuilder();

        builder.Expand((SyntaxNode)statement);
        var result = builder.BuildFreestanding();

        result.Node.ShouldBeSameAs(statement);
        result.Statement.ShouldBeSameAs(statement);
    }
}
