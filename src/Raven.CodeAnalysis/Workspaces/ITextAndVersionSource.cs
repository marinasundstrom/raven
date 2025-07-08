using System.Diagnostics.CodeAnalysis;

namespace Raven.CodeAnalysis;

public interface ITextAndVersionSource
{
    bool TryGetValue([NotNullWhen(true)] out TextAndVersion? textAndVersion);

    TextLoader TextLoader { get; }
}
