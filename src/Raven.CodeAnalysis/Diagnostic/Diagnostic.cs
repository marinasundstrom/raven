namespace Raven.CodeAnalysis;

public class Diagnostic : IEquatable<Diagnostic>
{
    private readonly object[]? _messageArgs;

    public DiagnosticDescriptor Descriptor { get; }

    public Location Location { get; }

    public object[] GetMessageArgs() => _messageArgs ?? [];

    public Diagnostic(DiagnosticDescriptor descriptor, Location location, object[]? messageArgs)
    {
        Descriptor = descriptor;
        Location = location;
        _messageArgs = messageArgs;
    }

    public override string ToString() => GetDescription();

    public static Diagnostic Create(DiagnosticDescriptor descriptor, Location location, params object[]? messageArgs)
    {
        return new Diagnostic(descriptor, location, messageArgs);
    }

    public string GetDescription()
        => $"{Descriptor.DefaultSeverity.ToString().ToLower()} {Descriptor.Id}: {GetMessage()}";

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

        // Compare the fully formatted message (args normalized via ProcessArgs)
        return string.Equals(GetMessage(), other.GetMessage(), StringComparison.Ordinal);
    }

    public override bool Equals(object? obj) => Equals(obj as Diagnostic);

    public override int GetHashCode()
    {
        var hash = new HashCode();
        hash.Add(Descriptor);
        hash.Add(Location);
        // Hash by final formatted message to align with Equals
        hash.Add(GetMessage(), StringComparer.Ordinal);
        return hash.ToHashCode();
    }

    public static bool operator ==(Diagnostic? left, Diagnostic? right) => Equals(left, right);
    public static bool operator !=(Diagnostic? left, Diagnostic? right) => !Equals(left, right);
}
