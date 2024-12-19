

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public class Diagnostic
{
    private readonly object[]? _messageArgs;

    public DiagnosticDescriptor Descriptor { get; }
    public Location Location { get; }

    public Diagnostic(DiagnosticDescriptor descriptor, Location location, object[]? messageArgs)
    {
        Descriptor = descriptor;
        Location = location;
        _messageArgs = messageArgs;
    }

    public override string ToString() => $"{Descriptor.Id}: {string.Format(Descriptor.MessageFormat.ToString(), _messageArgs ?? [])}";

    public static Diagnostic Create(DiagnosticDescriptor descriptor, Location location, object[]? messageArgs = null)
    {
        return new Diagnostic(descriptor, location, messageArgs);
    }
}

public enum DiagnosticSeverity
{
    Hidden = 0,
    Info = 1,
    Warning = 2,
    Error = 3
}
