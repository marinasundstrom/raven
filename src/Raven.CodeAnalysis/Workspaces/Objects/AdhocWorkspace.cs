namespace Raven.CodeAnalysis;

/// <summary>
/// A simple in-memory workspace used for tests and prototyping.
/// </summary>
public sealed class AdhocWorkspace : Workspace
{
    public AdhocWorkspace() : base("Adhoc")
    {
    }
}
