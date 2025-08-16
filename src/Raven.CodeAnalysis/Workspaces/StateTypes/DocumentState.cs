using System;
using System.Threading;
using System.Threading.Tasks;
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
        if (TextAndVersionSource.TryGetValue(out var cached))
            return cached.Text;

        var result = await TextAndVersionSource.TextLoader.LoadTextAndVersionAsync(CancellationToken.None);
        if (TextAndVersionSource is TextAndVersionSource concrete)
            concrete.SetValue(result);
        return result.Text;
    }

    private async Task<SyntaxTree> LoadSyntaxTreeAsync()
    {
        var text = await GetTextAsync();
        return SyntaxFactory.ParseSyntaxTree(text, ParseOptions);
    }

    public Task<SourceText> GetTextAsync(CancellationToken cancellationToken = default)
        => _lazyText.Value;

    public Task<SyntaxTree> GetSyntaxTreeAsync(CancellationToken cancellationToken = default)
        => _lazySyntaxTree.Value;

    public DocumentState WithText(SourceText newText)
    {
        if (newText == null) throw new ArgumentNullException(nameof(newText));

        var tav = new TextAndVersion(newText, VersionStamp.Create(), Info.FilePath);
        var source = new TextAndVersionSource(TextLoader.From(tav), tav);
        return new DocumentState(Info.WithText(newText), source, ParseOptions);
    }
}
