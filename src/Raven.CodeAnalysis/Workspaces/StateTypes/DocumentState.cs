using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using static Raven.CodeAnalysis.DocumentInfo;

namespace Raven.CodeAnalysis;

sealed class DocumentState : TextDocumentState
{
    private readonly Lazy<Task<SourceText>> _lazyText;
    private readonly Lazy<Task<SyntaxTree>> _lazySyntaxTree;

    public ParseOptions ParseOptions { get; }

    public DocumentState(DocumentAttributes attribute, ITextAndVersionSource textSource, ParseOptions parseOptions)
        : base(attribute, textSource)
    {
        _lazyText = new Lazy<Task<SourceText>>(LoadTextAsync);
        _lazySyntaxTree = new Lazy<Task<SyntaxTree>>(LoadSyntaxTreeAsync);
        ParseOptions = parseOptions;
    }

    private async Task<SourceText> LoadTextAsync()
    {
        if (Info.Text is not null)
            return Info.Text;
        if (Info.TextLoader is not null)
            return await Info.TextLoader.LoadTextAsync(CancellationToken.None);
        return SourceText.From(string.Empty);
    }

    private async Task<SyntaxTree> LoadSyntaxTreeAsync()
    {
        var text = await GetTextAsync();
        return SyntaxFactory.ParseSyntaxTree(text, Info.ParseOptions);
    }

    public Task<SourceText> GetTextAsync(CancellationToken cancellationToken = default)
        => _lazyText.Value;

    public Task<SyntaxTree> GetSyntaxTreeAsync(CancellationToken cancellationToken = default)
        => _lazySyntaxTree.Value;

    public DocumentState WithText(SourceText newText)
    {
        if (newText == null) throw new ArgumentNullException(nameof(newText));
        var newInfo = Info.WithText(newText);
        return new DocumentState(newInfo);
    }
}
