

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public class Diagnostic
{
    public DiagnosticDescriptor Descriptor { get; }
    public Location Location { get; }

    private Diagnostic(DiagnosticDescriptor descriptor, Location location)
    {
        Descriptor = descriptor;
        Location = location;
    }

    public override string ToString() => $"{Descriptor.Id}: {Descriptor.MessageFormat}";

    public static Diagnostic Create(DiagnosticDescriptor descriptor, Location location)
    {
        return new Diagnostic(descriptor, location);
    }

    internal static Diagnostic Create(object semicolonExpected, object value)
    {
        throw new NotImplementedException();
    }
}

public enum DiagnosticSeverity
{
    Hidden = 0,
    Info = 1,
    Warning = 2,
    Error = 3
}
