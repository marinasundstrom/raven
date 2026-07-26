using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Documentation;

/// <summary>
/// Represents documentation independently of its authored or projected format.
/// </summary>
public sealed class RavenDocumentation
{
    internal RavenDocumentation(
        DocumentationFormat sourceFormat,
        string sourceText,
        ImmutableArray<DocumentationSection> sections,
        ImmutableArray<DocumentationAssociation> associations,
        string? inheritedFrom)
    {
        SourceFormat = sourceFormat;
        SourceText = sourceText;
        Sections = sections;
        Associations = associations;
        InheritedFrom = inheritedFrom;
    }

    /// <summary>Gets the format from which the documentation was loaded.</summary>
    public DocumentationFormat SourceFormat { get; }

    /// <summary>Gets the original authored or loaded content.</summary>
    public string SourceText { get; }

    /// <summary>Gets the ordered narrative sections.</summary>
    public ImmutableArray<DocumentationSection> Sections { get; }

    /// <summary>Gets documentation associated with named or referenced subjects.</summary>
    public ImmutableArray<DocumentationAssociation> Associations { get; }

    /// <summary>Gets the optional symbol reference from which documentation is inherited.</summary>
    public string? InheritedFrom { get; }

    /// <summary>Gets the content for the first section with the specified <paramref name="kind"/>.</summary>
    /// <param name="kind">The narrative role to find.</param>
    /// <returns>The section content, or <see langword="null"/> when the role is absent.</returns>
    public string? GetSection(DocumentationSectionKind kind)
        => Sections.FirstOrDefault(section => section.Kind == kind)?.Content;

    /// <summary>Gets all subject associations with the specified <paramref name="kind"/>.</summary>
    /// <param name="kind">The association role to select.</param>
    /// <returns>The matching associations in source order.</returns>
    public ImmutableArray<DocumentationAssociation> GetAssociations(DocumentationAssociationKind kind)
        => Associations.Where(association => association.Kind == kind).ToImmutableArray();
}

/// <summary>Identifies a narrative role in Raven documentation.</summary>
public enum DocumentationSectionKind
{
    /// <summary>A concise introduction to the documented symbol.</summary>
    Summary,

    /// <summary>Additional narrative content that follows the summary.</summary>
    Details,

    /// <summary>The result produced by an operation.</summary>
    Result,

    /// <summary>The value represented by a property or other value-bearing symbol.</summary>
    Value,

    /// <summary>Usage notes or extended discussion.</summary>
    Remarks,

    /// <summary>An example of using the documented symbol.</summary>
    Example
}

/// <summary>Represents an ordered narrative section.</summary>
/// <param name="Kind">The semantic role of the section.</param>
/// <param name="Content">The authored section content.</param>
public sealed record DocumentationSection(DocumentationSectionKind Kind, string Content);

/// <summary>Identifies the subject associated with a documentation fragment.</summary>
public enum DocumentationAssociationKind
{
    /// <summary>A generic type parameter.</summary>
    TypeParameter,

    /// <summary>A callable or indexer parameter.</summary>
    Parameter,

    /// <summary>An error or exceptional outcome.</summary>
    Error,

    /// <summary>A directly referenced symbol or resource.</summary>
    Link,

    /// <summary>A related symbol or resource.</summary>
    RelatedLink
}

/// <summary>Associates documentation with a name or symbol reference.</summary>
/// <param name="Kind">The semantic role of the association.</param>
/// <param name="Name">The associated source-level name, when applicable.</param>
/// <param name="Reference">The associated symbol or resource reference, when applicable.</param>
/// <param name="Content">The authored description of the association.</param>
public sealed record DocumentationAssociation(
    DocumentationAssociationKind Kind,
    string? Name,
    string? Reference,
    string Content);
