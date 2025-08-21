using System;

namespace Raven.CodeAnalysis;

/// <summary>
/// Provides application-wide services for workspaces and solutions.
/// </summary>
public class HostServices
{
    public HostServices(
        SyntaxTreeProvider syntaxTreeProvider,
        PersistenceService? persistenceService = null)
    {
        SyntaxTreeProvider = syntaxTreeProvider ?? throw new ArgumentNullException(nameof(syntaxTreeProvider));
        PersistenceService = persistenceService;
    }

    /// <summary>The <see cref="SyntaxTreeProvider"/> used to parse documents.</summary>
    public SyntaxTreeProvider SyntaxTreeProvider { get; }

    /// <summary>The <see cref="PersistenceService"/> used to save and open workspaces.</summary>
    public PersistenceService? PersistenceService { get; }

    /// <summary>Gets a default instance of <see cref="HostServices"/>.</summary>
    public static HostServices Default { get; } = new HostServices(new SyntaxTreeProvider());
}
