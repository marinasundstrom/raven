using System;
using System.Threading;

using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Macros;

public sealed class MacroTokenStreamContext
{
    internal MacroTokenStreamContext(
        string bodyText,
        TextSpan bodySpan,
        ParseOptions parseOptions,
        CancellationToken cancellationToken)
    {
        BodyText = bodyText ?? throw new ArgumentNullException(nameof(bodyText));
        BodySpan = bodySpan;
        ParseOptions = parseOptions ?? throw new ArgumentNullException(nameof(parseOptions));
        CancellationToken = cancellationToken;
    }

    public string BodyText { get; }

    public TextSpan BodySpan { get; }

    public ParseOptions ParseOptions { get; }

    public CancellationToken CancellationToken { get; }
}
