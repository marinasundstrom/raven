using System.Threading;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

public sealed class Document : TextDocument
{
    private SyntaxTree? _lazySyntaxTree;
    private SemanticModel? _lazySemanticModel;

    internal Document(Project project, DocumentState state) : base(project, state)
    {

    }

    public Document WithName(string newName)
        => Project.Solution.WithDocumentName(Id, newName).GetDocument(Id)!;

    public Document WithText(SourceText newText)
        => Project.Solution.WithDocumentText(Id, newText).GetDocument(Id)!;

    public SyntaxTree GetSyntaxTree()
    {
        if (_lazySyntaxTree is not null)
            return _lazySyntaxTree;

        var text = GetTextAsync(CancellationToken.None).GetAwaiter().GetResult();
        _lazySyntaxTree = SyntaxTree.ParseText(text);
        return _lazySyntaxTree;
    }

    public SemanticModel GetSemanticModel(Compilation compilation)
    {
        if (_lazySemanticModel is not null)
            return _lazySemanticModel;

        _lazySemanticModel = compilation.GetSemanticModel(GetSyntaxTree());
        return _lazySemanticModel;
    }
}
