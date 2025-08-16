using System;
using System.Threading;
using System.Threading.Tasks;

namespace Raven.CodeAnalysis;

/// <summary>
/// Simple caching wrapper around a text/verssion loader.
/// </summary>
internal sealed class TextAndVersionSource : ITextAndVersionSource
{
    private readonly Func<CancellationToken, Task<TextAndVersion>> _loader;
    private TextAndVersion? _cached;

    public TextAndVersionSource(Func<CancellationToken, Task<TextAndVersion>> loader)
    {
        _loader = loader;
    }

    public async Task<TextAndVersion> GetValueAsync(CancellationToken cancellationToken)
    {
        if (_cached == null)
            _cached = await _loader(cancellationToken).ConfigureAwait(false);
        return _cached;
    }
}

