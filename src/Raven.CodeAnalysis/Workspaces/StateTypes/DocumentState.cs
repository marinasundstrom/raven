using System.Threading;
using System.Threading.Tasks;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using static Raven.CodeAnalysis.DocumentInfo;

namespace Raven.CodeAnalysis;

sealed class DocumentState : TextDocumentState
{
    private readonly ParseOptions _parseOptions;
    private SyntaxTree? _syntaxTree;

    public DocumentState(DocumentAttributes attributes, ITextAndVersionSource textSource, ParseOptions parseOptions)
        : base(attributes, textSource)
    {
        _parseOptions = parseOptions;
    }

    public async Task<SyntaxTree> GetSyntaxTreeAsync(CancellationToken cancellationToken = default)
    {
        if (_syntaxTree is not null)
            return _syntaxTree;

        var text = await GetTextAsync(cancellationToken);
        _syntaxTree = SyntaxFactory.ParseSyntaxTree(text, _parseOptions);
        return _syntaxTree;
    }

    public DocumentState WithText(SourceText newText)
    {
        var newAttributes = Attributes.WithText(newText);
        return new DocumentState(newAttributes, TextAndVersionSource, _parseOptions);
    }
}
