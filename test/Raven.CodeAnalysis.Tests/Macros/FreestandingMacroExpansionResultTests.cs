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
        var result = builder.BuildInvocable();

        result.Node.ShouldBeSameAs(statement);
        result.Statement.ShouldBeSameAs(statement);
    }

    [Fact]
    public void ExpressionAndStatementProperties_AreExclusiveTypedProjections()
    {
        var result = FreestandingMacroExpansionResult.FromExpression(
            SyntaxFactory.ParseExpression("42"));

        result.Statement = SyntaxFactory.ParseStatement("return 42");

        result.Expression.ShouldBeNull();
        result.Statement.ShouldNotBeNull();
        result.Node.ShouldBeOfType<ReturnStatementSyntax>();
    }

    [Fact]
    public void FromMembers_SelectsOrderedMemberListOutput()
    {
        var first = ParseMember("class First {}");
        var second = ParseMember("class Second {}");

        var result = FreestandingMacroExpansionResult.FromMembers(
            SyntaxFactory.List<MemberDeclarationSyntax>([first, second]));

        result.HasMemberExpansion.ShouldBeTrue();
        result.Node.ShouldBeNull();
        result.Members.Select(static member => member.ToString())
            .ShouldBe(["class First {}", "class Second {}"]);
    }

    [Fact]
    public void EmptyMemberList_RemainsAnExplicitExpansion()
    {
        var result = FreestandingMacroExpansionResult.FromMembers(
            SyntaxFactory.List<MemberDeclarationSyntax>());

        result.HasMemberExpansion.ShouldBeTrue();
        result.Members.ShouldBeEmpty();
        result.ShouldNotBeSameAs(FreestandingMacroExpansionResult.Empty);
    }

    [Fact]
    public void SingleNodeAndMemberListProperties_AreMutuallyExclusive()
    {
        var member = ParseMember("class Generated {}");
        var result = FreestandingMacroExpansionResult.FromExpression(
            SyntaxFactory.ParseExpression("42"));

        result.Members = [member];

        result.HasMemberExpansion.ShouldBeTrue();
        result.Node.ShouldBeNull();

        result.Expression = SyntaxFactory.ParseExpression("43");

        result.HasMemberExpansion.ShouldBeFalse();
        result.Members.ShouldBeEmpty();
        result.Expression.ShouldNotBeNull();
    }

    [Fact]
    public void Builder_PreservesExplicitEmptyMemberExpansion()
    {
        var builder = new MacroExpansionResultBuilder();

        builder.Expand(SyntaxFactory.List<MemberDeclarationSyntax>());
        var result = builder.BuildInvocable();

        result.HasMemberExpansion.ShouldBeTrue();
        result.Members.ShouldBeEmpty();
        result.ShouldNotBeSameAs(FreestandingMacroExpansionResult.Empty);
    }

    private static MemberDeclarationSyntax ParseMember(string source)
        => SyntaxFactory.ParseSyntaxTree(source).GetRoot().Members.Single();
}
