using System;

namespace Raven.CodeAnalysis;

/// <summary>
/// Provides application-wide services for workspaces and solutions.
/// </summary>
public class HostServices
{
    public HostServices(SyntaxTreeProvider syntaxTreeProvider)
    {
        SyntaxTreeProvider = syntaxTreeProvider ?? throw new ArgumentNullException(nameof(syntaxTreeProvider));
    }

    /// <summary>The <see cref="SyntaxTreeProvider"/> used to parse documents.</summary>
    public SyntaxTreeProvider SyntaxTreeProvider { get; }

    /// <summary>Gets a default instance of <see cref="HostServices"/>.</summary>
    public static HostServices Default { get; } = new HostServices(new SyntaxTreeProvider());
}
