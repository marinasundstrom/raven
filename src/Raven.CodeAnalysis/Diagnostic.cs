namespace Raven.CodeAnalysis;

public class Diagnostic : IEquatable<Diagnostic>
{
    private readonly object[]? _messageArgs;

    public DiagnosticDescriptor Descriptor { get; }

    public Location Location { get; }

    public DiagnosticSeverity Severity { get; }

    public object[] GetMessageArgs() => _messageArgs ?? [];

    public Diagnostic(DiagnosticDescriptor descriptor, Location location, object[]? messageArgs, DiagnosticSeverity? severity = null)
    {
        Descriptor = descriptor;
        Location = location;
        _messageArgs = messageArgs;
        Severity = severity ?? descriptor.DefaultSeverity;
    }

    public override string ToString() => GetDescription();

    public static Diagnostic Create(DiagnosticDescriptor descriptor, Location location, params object[]? messageArgs)
    {
        return new Diagnostic(descriptor, location, messageArgs);
    }

    public static Diagnostic Create(DiagnosticDescriptor descriptor, Location location, DiagnosticSeverity severity, params object[]? messageArgs)
    {
        return new Diagnostic(descriptor, location, messageArgs, severity);
    }

    public string GetDescription()
        => $"{Severity.ToString().ToLower()} {Descriptor.Id}: {GetMessage()}";

    public string GetMessage()
        => string.Format(Descriptor.MessageFormat, _messageArgs is not null ? ProcessArgs(_messageArgs).ToArray() : []);

    private IEnumerable<object> ProcessArgs(object[]? messageArgs)
    {
        return messageArgs?.Select(arg =>
        {
            if (arg is ISymbol symbol)
                return symbol.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);

            return arg;
        }) ?? [];
    }

    internal static Diagnostic Create(object memberAccessOnVoid, Location location)
    {
        throw new NotImplementedException();
    }

    // ---- Equality ----

    public bool Equals(Diagnostic? other)
    {
        if (ReferenceEquals(this, other))
            return true;
        if (other is null)
            return false;

        // Descriptor and Location are expected to implement meaningful equality
        if (!Equals(Descriptor, other.Descriptor))
            return false;

        if (!Equals(Location, other.Location))
            return false;

        if (Severity != other.Severity)
            return false;

        // Compare the fully formatted message (args normalized via ProcessArgs)
        return string.Equals(GetMessage(), other.GetMessage(), StringComparison.Ordinal);
    }

    public override bool Equals(object? obj) => Equals(obj as Diagnostic);

    public override int GetHashCode()
    {
        var hash = new HashCode();
        hash.Add(Descriptor);
        hash.Add(Location);
        hash.Add(Severity);
        // Hash by final formatted message to align with Equals
        hash.Add(GetMessage(), StringComparer.Ordinal);
        return hash.ToHashCode();
    }

    public static bool operator ==(Diagnostic? left, Diagnostic? right) => Equals(left, right);
    public static bool operator !=(Diagnostic? left, Diagnostic? right) => !Equals(left, right);

    internal Diagnostic WithSeverity(DiagnosticSeverity severity)
        => new Diagnostic(Descriptor, Location, GetMessageArgs(), severity);
}
