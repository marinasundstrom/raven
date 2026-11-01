namespace Raven.CodeAnalysis;

public class DiagnosticDescriptor : IEquatable<DiagnosticDescriptor>
{
    private DiagnosticDescriptor(
        string id,
        LocalizableString title,
        LocalizableString? description,
        string helpLinkUri,
        LocalizableString messageFormat,
        string category,
        DiagnosticSeverity defaultSeverity,
        bool isEnabledByDefault)
    {
        Id = id;
        Title = title;
        Description = description;
        HelpLinkUri = helpLinkUri;
        MessageFormat = messageFormat;
        Category = category;
        DefaultSeverity = defaultSeverity;
        IsEnabledByDefault = isEnabledByDefault;
    }

    public static DiagnosticDescriptor Create(
        string id,
        LocalizableString title,
        LocalizableString? description,
        string helpLinkUri,
        LocalizableString messageFormat,
        string category,
        DiagnosticSeverity defaultSeverity,
        bool isEnabledByDefault = true)
    {
        return new DiagnosticDescriptor(id, title, description, helpLinkUri, messageFormat, category, defaultSeverity, isEnabledByDefault);
    }

    public string Id { get; }

    public LocalizableString Title { get; }

    public LocalizableString? Description { get; }

    public string HelpLinkUri { get; }

    public LocalizableString MessageFormat { get; }

    public string Category { get; }

    public DiagnosticSeverity DefaultSeverity { get; }

    public bool IsEnabledByDefault { get; }

    public override bool Equals(object? obj)
    {
        return obj is DiagnosticDescriptor other && Equals(other);
    }

    public bool Equals(DiagnosticDescriptor? other) => other is not null && Id == other.Id;

    public static bool operator ==(DiagnosticDescriptor? left, DiagnosticDescriptor? right) =>
        ReferenceEquals(left, right) || (left is not null && left.Equals(right));

    public static bool operator !=(DiagnosticDescriptor? left, DiagnosticDescriptor? right) => !(left == right);

    public override int GetHashCode()
    {
        return Id.GetHashCode();
    }
}
