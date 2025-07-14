using System.Diagnostics.CodeAnalysis;

namespace Raven.CodeAnalysis;

public sealed class TextAndVersionSource : ITextAndVersionSource
{
    public TextLoader TextLoader => throw new NotImplementedException();

    public bool TryGetValue([NotNullWhen(true)] out TextAndVersion textAndVersion)
    {
        throw new NotImplementedException();
    }
}
