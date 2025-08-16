using System.Threading;
using System.Threading.Tasks;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

public sealed class Document : TextDocument
{
    private SemanticModel? _lazySemanticModel;

    internal Document(Project project, DocumentState state) : base(project, state)
    {

    }

    public Document WithName(string newName)
        => Project.Solution.WithDocumentName(Id, newName).GetDocument(Id)!;

    public Document WithText(SourceText newText)
        => Project.Solution.WithDocumentText(Id, newText).GetDocument(Id)!;

    public Task<SyntaxTree> GetSyntaxTreeAsync(CancellationToken cancellationToken = default)
        => State.GetSyntaxTreeAsync(cancellationToken);

    public async Task<SemanticModel> GetSemanticModelAsync(Compilation compilation, CancellationToken cancellationToken = default)
    {
        if (_lazySemanticModel is not null)
            return _lazySemanticModel;

        var tree = await GetSyntaxTreeAsync(cancellationToken);
        _lazySemanticModel = compilation.GetSemanticModel(tree);
        return _lazySemanticModel;
    }
}
