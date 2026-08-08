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

    [Fact]
    public void GetMacroTokens_NormalizesFailingOptionalMetadata()
    {
        const string code = "import Raven.CodeAnalysis.Tests.Macros.*\nlet value = #resilient { from users }";
        var (compilation, expression) = CreateCompilation(code, new ResilientMacro());

        var tokens = compilation.GetMacroTokens(expression);

        Assert.Collection(
            tokens,
            token =>
            {
                Assert.Equal(MacroTokenClassification.Keyword, token.Classification);
                Assert.Null(token.KindName);
                Assert.Null(token.Symbol);
            },
            token =>
            {
                Assert.Equal(MacroTokenClassification.Default, token.Classification);
                Assert.Equal(nameof(SyntaxKind.IdentifierToken), token.KindName);
                Assert.Null(token.Symbol);
            });
    }

    [Fact]
    public void GetMacroTokens_ProjectsOrdinarySymbolTargets()
    {
        const string code = "import Raven.CodeAnalysis.Tests.Macros.*\nclass Greeting { }\nlet value = #symbols { <Greeting }";
        var (compilation, expression) = CreateCompilation(code, new SymbolMacro());

        var token = Assert.Single(
            compilation.GetMacroTokens(expression),
            static candidate => candidate.Text == "Greeting");

        var symbol = Assert.IsAssignableFrom<INamedTypeSymbol>(token.Symbol);
        Assert.Equal("Greeting", symbol.Name);
        Assert.Contains(symbol.Locations, static location => location.IsInSource);
        Assert.Same(token, compilation.GetMacroInputSnapshot(expression).FindToken(token.Span.Start));
    }

    [Fact]
    public void MacroFunctionTokenContributions_ProjectThroughGeneratedAdapter()
    {
        const string code = """
            import Raven.CodeAnalysis.Macros.*

            macro func Classified(tokens: IMacroTokenStream, context: TokenTreeMacroContext) {
                let keyword = tokens.ReadToken()
                token context.CreateTokenInfo(keyword, "DslKeyword", MacroTokenClassification.Keyword)
                let identifier = tokens.ReadToken()
                token context.CreateTokenInfo(identifier, "DslIdentifier", MacroTokenClassification.Identifier)
                expand Raven.CodeAnalysis.Syntax.SyntaxFactory.ParseExpression("0")
            }

            func Main() -> int => Classified! { select customer }
            """;
        var authoredTree = SyntaxTree.ParseText(code, path: "main.rvn");
        var compilation = Compilation.Create(
                "MacroFunctionTokens",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros)
            .AddSyntaxTreesWithLocalMacros(authoredTree);
        var consumerTree = Assert.Single(compilation.SyntaxTrees);
        var expression = consumerTree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();

        var tokens = compilation.GetMacroTokens(expression);

        Assert.Collection(
            tokens,
            token =>
            {
                Assert.Equal("select", token.Text);
                Assert.Equal("DslKeyword", token.KindName);
                Assert.Equal(MacroTokenClassification.Keyword, token.Classification);
            },
            token =>
            {
                Assert.Equal("customer", token.Text);
                Assert.Equal("DslIdentifier", token.KindName);
                Assert.Equal(MacroTokenClassification.Identifier, token.Classification);
            });
    }

    [Fact]
    public void GetMacroTokens_DoesNotExpandMacroWithoutSourceMetadataContributions()
    {
        const string code = "import Raven.CodeAnalysis.Tests.Macros.*\nlet value = plain! { value }";
        var (compilation, expression) = CreateCompilation(code, new ExpansionFailingMacro());

        var tokens = compilation.GetMacroTokens(expression);

        Assert.Equal("value", Assert.Single(tokens).Text);
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

    private sealed class ResilientMacro :
        ITokenTreeExpressionMacro,
        IMacroKeywordProvider,
        IMacroTokenKindProvider,
        IMacroTokenClassifier,
        IMacroTokenSymbolProvider
    {
        public string Name => "resilient";

        public ImmutableArray<MacroKeyword> Keywords =>
            [new MacroKeyword("from", QueryMacro.FromRawKind)];

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;

        public string? GetTokenKindName(int rawKind)
            => throw new InvalidOperationException("broken kind name provider");

        public MacroTokenClassification ClassifyToken(
            TokenTreeMacroContext context,
            SyntaxToken token)
            => token.RawKind == QueryMacro.FromRawKind
                ? throw new InvalidOperationException("broken classifier")
                : (MacroTokenClassification)int.MaxValue;

        public ISymbol? GetTokenSymbol(TokenTreeMacroContext context, SyntaxToken token)
            => throw new InvalidOperationException("broken symbol provider");
    }

    private sealed class SymbolMacro : ITokenTreeExpressionMacro, IMacroTokenSymbolProvider
    {
        public string Name => "symbols";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => FreestandingMacroExpansionResult.Empty;

        public ISymbol? GetTokenSymbol(TokenTreeMacroContext context, SyntaxToken token)
            => context.Compilation.GetTypeByMetadataName(token.ValueText);
    }

    private sealed class ExpansionFailingMacro : ITokenTreeExpressionMacro
    {
        public string Name => "plain";

        public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
            => throw new InvalidOperationException("Token discovery must not expand this macro.");
    }
}
