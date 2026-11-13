using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests.Symbols;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "Shared")]
public sealed class MacroValueContractTests
{
    [Theory]
    [InlineData(DiagnosticSeverity.Error)]
    [InlineData(DiagnosticSeverity.Warning)]
    [InlineData(DiagnosticSeverity.Info)]
    [InlineData(DiagnosticSeverity.Hidden)]
    public void DiagnosticFactoriesPreservePayloadAndValueEquality(DiagnosticSeverity severity)
    {
        var location = Location.None;
        var actual = severity switch
        {
            DiagnosticSeverity.Error => MacroExpansionDiagnostic.Error("message", location, "CODE"),
            DiagnosticSeverity.Warning => MacroExpansionDiagnostic.Warning("message", location, "CODE"),
            DiagnosticSeverity.Info => MacroExpansionDiagnostic.Info("message", location, "CODE"),
            _ => MacroExpansionDiagnostic.Hidden("message", location, "CODE")
        };
        var expected = new MacroExpansionDiagnostic(severity, "message", location, "CODE");
        Assert.True(expected == actual);
        Assert.False(expected != actual);
        Assert.Equal(expected.GetHashCode(), actual.GetHashCode());
        Assert.Same(location, actual.Location);
        Assert.Equal(severity, actual.Severity);
        Assert.Equal("message", actual.Message);
        Assert.Equal("CODE", actual.Code);
        var otherSeverity = severity == DiagnosticSeverity.Error ? DiagnosticSeverity.Warning : DiagnosticSeverity.Error;
        Assert.NotEqual(new MacroExpansionDiagnostic(otherSeverity, "message", location, "CODE"), actual);
        Assert.NotEqual(new MacroExpansionDiagnostic(severity, "message", null, "CODE"), actual);
        Assert.NotEqual(new MacroExpansionDiagnostic(severity, "different", location, "CODE"), actual);
        Assert.NotEqual(new MacroExpansionDiagnostic(severity, "message", location, "OTHER"), actual);
    }

    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData(" \t")]
    public void DiagnosticRejectsMissingMessages(string? message)
        => Assert.Equal("message", Assert.Throws<ArgumentException>(() => MacroExpansionDiagnostic.Error(message!)).ParamName);

    [Fact]
    public void DiagnosticOptionalPayloadRemainsAbsent()
    {
        var diagnostic = MacroExpansionDiagnostic.Info("message");
        Assert.Null(diagnostic.Location);
        Assert.Null(diagnostic.Code);
    }

    [Fact]
    public void ExpressionFacadeRetainsSyntaxTypeAndValueEquality()
    {
        var syntax = SyntaxFactory.ParseExpression("42");
        var type = MetadataSymbolFixture.Make<TypeSymbol>(SymbolKind.Type, "Int32", TypeKind.Struct);
        var first = new ExpressionSyntax<int>(syntax, type);
        var second = new ExpressionSyntax<int>(syntax, type);
        Assert.Same(syntax, first.Syntax);
        Assert.Same(type, first.Type);
        Assert.True(first == second);
        Assert.Equal(first.GetHashCode(), second.GetHashCode());
        Assert.False(first == new ExpressionSyntax<int>(SyntaxFactory.ParseExpression("43"), type));
        var otherType = MetadataSymbolFixture.Make<TypeSymbol>(SymbolKind.Type, "Int64", TypeKind.Struct);
        Assert.True(first != new ExpressionSyntax<int>(syntax, otherType));
        Assert.False(first.Equals(new ExpressionSyntax<string>(syntax, type)));
    }

    [Fact]
    public void TokenStreamContractPreservesPeekAndReadShapes()
    {
        var peek = typeof(IMacroTokenStream).GetMethod(nameof(IMacroTokenStream.PeekToken))!;
        Assert.Equal(typeof(SyntaxToken), peek.ReturnType);
        Assert.Equal(0, Assert.Single(peek.GetParameters()).DefaultValue);
        Assert.Equal(typeof(SyntaxToken), typeof(IMacroTokenStream).GetMethod(nameof(IMacroTokenStream.ReadToken))!.ReturnType);
        Assert.Equal(typeof(bool), typeof(IMacroTokenStream).GetProperty(nameof(IMacroTokenStream.IsEndOfFile))!.PropertyType);
    }
}
