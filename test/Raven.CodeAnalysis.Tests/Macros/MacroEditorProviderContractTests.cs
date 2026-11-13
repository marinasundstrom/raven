using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Macros.Tests;

public sealed partial class TokenTreeMacroContextTests
{
    [Fact]
    public void EditorProviders_DispatchWithAuthoredContextAndCoordinates()
    {
        var context = CreateContext("value");
        var token = context.CreateTokenStream().ReadToken();
        var provider = new EditorProvider();
        Assert.IsAssignableFrom<IMacroExpansionMetadataProvider>(provider);
        Assert.Equal("custom", ((IMacroTokenKindProvider)provider).GetTokenKindName(90001));
        Assert.Equal(MacroTokenClassification.Identifier, ((IMacroTokenClassifier)provider).ClassifyToken(context, token));
        Assert.Same(context, provider.LastContext);
        Assert.Equal(token, provider.LastToken);
        Assert.Null(((IMacroTokenSymbolProvider)provider).GetTokenSymbol(context, token));
        var region = Assert.Single(((IMacroFragmentProvider)provider).GetFragmentRegions(context));
        Assert.Equal(context.BodySpan, region.Span);
        Assert.NotNull(((IMacroEmbeddedLanguageProvider)provider).GetEmbeddedLanguageProjection(context));
        var completion = Assert.Single(((IMacroCompletionProvider)provider).GetCompletions(context, 2));
        Assert.Equal(new TextSpan(2, 0), completion.BodyRelativeReplacementSpan);
        Assert.Equal("item", completion.InsertionText);
        Assert.Same(context, provider.LastContext);
    }

    [Fact]
    public void CompletionItem_PreservesRecordValueAndInitMetadata()
    {
        var item = new MacroCompletionItem("display", "insert", new TextSpan(1, 2));
        Assert.Null(item.CursorOffset);
        Assert.Null(item.Description);
        Assert.Null(item.Symbol);
        Assert.Equal(item, new MacroCompletionItem("display", "insert", new TextSpan(1, 2)));
        Assert.Equal(item.GetHashCode(), new MacroCompletionItem("display", "insert", new TextSpan(1, 2)).GetHashCode());
        var changed = item with { CursorOffset = 3 };
        Assert.NotEqual(item, changed);
        Assert.Null(item.CursorOffset);
        Assert.Contains(typeof(System.Runtime.CompilerServices.IsExternalInit),
            typeof(MacroCompletionItem).GetProperty("Description")!.SetMethod!.ReturnParameter.GetRequiredCustomModifiers());
    }

    private sealed class EditorProvider : IMacroTokenKindProvider, IMacroTokenClassifier,
        IMacroTokenSymbolProvider, IMacroFragmentProvider, IMacroEmbeddedLanguageProvider,
        IMacroCompletionProvider, IMacroExpansionMetadataProvider
    {
        public TokenTreeMacroContext? LastContext { get; private set; }
        public SyntaxToken LastToken { get; private set; }

        public string? GetTokenKindName(int rawKind) => rawKind == 90001 ? "custom" : null;

        public MacroTokenClassification ClassifyToken(TokenTreeMacroContext context, SyntaxToken token)
        {
            LastContext = context;
            LastToken = token;
            return MacroTokenClassification.Identifier;
        }

        public Symbol? GetTokenSymbol(TokenTreeMacroContext context, SyntaxToken token) => null;

        public ImmutableArray<MacroFragmentRegion> GetFragmentRegions(TokenTreeMacroContext context)
            => [context.CreateFragmentRegion(MacroFragmentKind.Expression, new TextSpan(0, context.BodySpan.Length))];

        public MacroEmbeddedLanguageProjection? GetEmbeddedLanguageProjection(TokenTreeMacroContext context)
            => context.CreateEmbeddedLanguageProjection("test", context.GetBodyText());

        public ImmutableArray<MacroCompletionItem> GetCompletions(TokenTreeMacroContext context, int bodyRelativePosition)
        {
            LastContext = context;
            return [new MacroCompletionItem("item", "item", new TextSpan(bodyRelativePosition, 0))];
        }
    }
}
