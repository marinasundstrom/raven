using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

public sealed class Document : TextDocument
{
    internal Document(DocumentState state) : base(state) { }

    internal new DocumentState State => (DocumentState)base.State;

    public Document WithText(SourceText newText) => new(State.WithText(newText));

    public Document WithName(string newName) => new(State.WithName(newName));
}

