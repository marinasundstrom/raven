
using static Raven.CodeAnalysis.DocumentInfo;
using static Raven.CodeAnalysis.SolutionInfo;

namespace Raven.CodeAnalysis;

class SolutionCompilationState
{
    private string? _kind;
    private SolutionAttributes _attributes;

    public SolutionCompilationState(string? kind, SolutionAttributes attributes)
    {
        _kind = kind;
        _attributes = attributes;
    }

    public SolutionState SolutionState { get; internal set; }

    internal SolutionCompilationState WithDocumentAttributes(DocumentId documentId, string name, Func<DocumentAttributes, string, object> value)
    {
        throw new NotImplementedException();
    }
}
