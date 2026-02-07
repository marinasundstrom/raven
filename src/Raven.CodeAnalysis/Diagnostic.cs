using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

public class Diagnostic : IEquatable<Diagnostic>
{
    private readonly object[]? _messageArgs;
    private static readonly ImmutableDictionary<string, string?> EmptyProperties = ImmutableDictionary<string, string?>.Empty;

    public DiagnosticDescriptor Descriptor { get; }

    public Location Location { get; }

    public DiagnosticSeverity Severity { get; }

    public bool IsSuppressed { get; }

    public ImmutableDictionary<string, string?> Properties { get; }

    public string Id => Descriptor.Id;

    public object[] GetMessageArgs() => _messageArgs ?? [];

    public Diagnostic(
        DiagnosticDescriptor descriptor,
        Location location,
        object[]? messageArgs,
        DiagnosticSeverity? severity = null,
        bool isSuppressed = false,
        ImmutableDictionary<string, string?>? properties = null)
    {
        Descriptor = descriptor;
        Location = location ?? throw new ArgumentNullException(nameof(location));
        _messageArgs = messageArgs;
        Severity = severity ?? descriptor.DefaultSeverity;
        IsSuppressed = isSuppressed;
        Properties = properties ?? EmptyProperties;
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

    public static Diagnostic Create(
        DiagnosticDescriptor descriptor,
        Location location,
        ImmutableDictionary<string, string?> properties)
    {
        return new Diagnostic(descriptor, location, messageArgs: null, properties: properties);
    }

    public static Diagnostic Create(
        DiagnosticDescriptor descriptor,
        Location location,
        DiagnosticSeverity severity,
        ImmutableDictionary<string, string?> properties)
    {
        return new Diagnostic(descriptor, location, messageArgs: null, severity: severity, properties: properties);
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

    internal static Diagnostic Create(object MemberAccessOnUnit, Location location)
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

        if (IsSuppressed != other.IsSuppressed)
            return false;

        if (!PropertiesEqual(Properties, other.Properties))
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
        hash.Add(IsSuppressed);
        foreach (var property in Properties.OrderBy(x => x.Key, StringComparer.Ordinal))
        {
            hash.Add(property.Key, StringComparer.Ordinal);
            hash.Add(property.Value, StringComparer.Ordinal);
        }

        // Hash by final formatted message to align with Equals
        hash.Add(GetMessage(), StringComparer.Ordinal);
        return hash.ToHashCode();
    }

    public static bool operator ==(Diagnostic? left, Diagnostic? right) => Equals(left, right);
    public static bool operator !=(Diagnostic? left, Diagnostic? right) => !Equals(left, right);

    internal Diagnostic WithSeverity(DiagnosticSeverity severity)
        => new Diagnostic(Descriptor, Location, GetMessageArgs(), severity, IsSuppressed, Properties);

    internal Diagnostic WithSuppression(bool isSuppressed)
        => new Diagnostic(Descriptor, Location, GetMessageArgs(), Severity, isSuppressed, Properties);

    private static bool PropertiesEqual(
        ImmutableDictionary<string, string?> left,
        ImmutableDictionary<string, string?> right)
    {
        if (left.Count != right.Count)
            return false;

        foreach (var property in left)
        {
            if (!right.TryGetValue(property.Key, out var value))
                return false;

            if (!string.Equals(property.Value, value, StringComparison.Ordinal))
                return false;
        }

        return true;
    }
}
