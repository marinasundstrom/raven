namespace Raven.CodeAnalysis;

public class DiagnosticDescriptor
{
    private DiagnosticDescriptor(string id, LocalizableString title, LocalizableString? description, string helpLinkUri, LocalizableString messageFormat, string category, DiagnosticSeverity defaultSeverity)
    {
        Id = id;
        Title = title;
        Description = description;
        HelpLinkUri = helpLinkUri;
        MessageFormat = messageFormat;
        Category = category;
        DefaultSeverity = defaultSeverity;
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
        return new DiagnosticDescriptor(id, title, description, helpLinkUri, messageFormat, category, defaultSeverity);
    }

    public string Id { get; }

    public LocalizableString Title { get; }

    public LocalizableString? Description { get; }

    public string HelpLinkUri { get; }

    public LocalizableString MessageFormat { get; }

    public string Category { get; }

    public DiagnosticSeverity DefaultSeverity { get; }

    public override bool Equals(object? obj)
    {
        return obj is DiagnosticDescriptor other && Id == other.Id;
    }

    public override int GetHashCode()
    {
        return Id.GetHashCode();
    }
}