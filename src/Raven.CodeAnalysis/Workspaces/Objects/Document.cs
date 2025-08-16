using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

/// <summary>
/// Facade for a single source document.  The immutable information is held in
/// <see cref="DocumentState"/>; this type provides convenient accessors.
/// </summary>
public sealed class Document : TextDocument
{
    private SyntaxTree? _lazySyntaxTree;

    internal Document(Project project, DocumentState state) : base(project, state)
    {
    }

    public SyntaxTree GetSyntaxTree()
    {
        if (_lazySyntaxTree is not null)
            return _lazySyntaxTree;

        _lazySyntaxTree = SyntaxTree.ParseText(State.Text);
        return _lazySyntaxTree;
    }
}
