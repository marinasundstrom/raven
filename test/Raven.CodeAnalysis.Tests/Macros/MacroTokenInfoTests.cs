using System;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Macros;

public sealed class MacroTokenInfoTests
{
    [Fact]
    public void GetMacroTokens_PreservesRawKindSpansAndKeywordClassification()
    {
        const string code = "import Raven.CodeAnalysis.Tests.Macros.*\nlet value = #query { from users }";
        var (compilation, expression) = CreateCompilation(code, new QueryMacro());

        var tokens = compilation.GetMacroTokens(expression);

        Assert.Collection(
            tokens,
            token =>
            {
                Assert.Equal(QueryMacro.FromRawKind, token.RawKind);
                Assert.Equal("FromKeyword", token.KindName);
                Assert.Equal("from", token.Text);
                Assert.Equal(MacroTokenClassification.Keyword, token.Classification);
                Assert.Equal("from", code.Substring(token.Span.Start, token.Span.Length));
            },
            token =>
            {
                Assert.Equal((int)SyntaxKind.IdentifierToken, token.RawKind);
                Assert.Equal(nameof(SyntaxKind.IdentifierToken), token.KindName);
                Assert.Equal("users", token.Text);
                Assert.Equal(MacroTokenClassification.Identifier, token.Classification);
                Assert.Equal(
                    token.Span.Start - expression.TokenTree!.OpenBraceToken.Span.End,
                    token.BodyRelativeSpan.Start);
            });
    }

    [Fact]
    public void GetMacroTokens_ReturnsEmptyWhenOptionalTokenProviderFails()
    {
        const string code = "import Raven.CodeAnalysis.Tests.Macros.*\nlet value = #broken { value }";
        var (compilation, expression) = CreateCompilation(code, new BrokenTokenMacro());

        var tokens = compilation.GetMacroTokens(expression);

        Assert.Empty(tokens);
    }

    private static (Compilation Compilation, FreestandingMacroExpressionSyntax Expression) CreateCompilation(
        string code,
        IMacroDefinition macro)
    {
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "MacroTokens",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddMacroReferences(new MacroReference(macro));
        var expression = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        return (compilation, expression);
    }

    private sealed class QueryMacro :
        ITokenTreeExpressionMacro,
        IMacroKeywordProvider,
        IMacroTokenKindProvider,
        IMacroTokenClassifier
    {
        public const int FromRawKind = 91001;

        public string Name => "query";

        public ImmutableArray<MacroKeyword> Keywords =>
            [new MacroKeyword("from", FromRawKind)];

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;

        public string? GetTokenKindName(int rawKind)
            => rawKind == FromRawKind ? "FromKeyword" : null;

        public MacroTokenClassification ClassifyToken(
            TokenTreeMacroContext context,
            SyntaxToken token)
            => token.Kind == SyntaxKind.IdentifierToken && token.RawKind == (int)SyntaxKind.IdentifierToken
                ? MacroTokenClassification.Identifier
                : MacroTokenClassification.Default;
    }

    private sealed class BrokenTokenMacro : ITokenTreeExpressionMacro, IMacroTokenStreamProvider
    {
        public string Name => "broken";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;

        public IMacroTokenStream CreateTokenStream(MacroTokenStreamContext context)
            => throw new InvalidOperationException("broken token provider");
    }
}
