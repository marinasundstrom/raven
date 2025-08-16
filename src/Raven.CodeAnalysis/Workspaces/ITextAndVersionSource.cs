using System.Threading;
using System.Threading.Tasks;

namespace Raven.CodeAnalysis;

/// <summary>
/// Provides access to a <see cref="TextAndVersion"/> value, potentially loading it lazily.
/// </summary>
internal interface ITextAndVersionSource
{
    Task<TextAndVersion> GetValueAsync(CancellationToken cancellationToken);
}

