using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Documentation;

/// <summary>Loads supported documentation inputs into Raven's format-neutral model.</summary>
public static class RavenDocumentationLoader
{
    /// <summary>Loads a documentation comment into Raven's format-neutral representation.</summary>
    /// <param name="documentation">The Markdown or XML documentation comment to load.</param>
    /// <returns>A Raven documentation value, or an empty value when <paramref name="documentation"/> is null.</returns>
    public static RavenDocumentation Load(DocumentationComment? documentation)
    {
        var structure = DocumentationStructureExtractor.Extract(documentation);
        var sections = ImmutableArray.CreateBuilder<DocumentationSection>();
        var associations = ImmutableArray.CreateBuilder<DocumentationAssociation>();

        AddSection(sections, DocumentationSectionKind.Summary, structure.Summary);
        AddSection(sections, DocumentationSectionKind.Details, structure.AdditionalBody);
        AddSection(sections, DocumentationSectionKind.Result, structure.Returns);
        AddSection(sections, DocumentationSectionKind.Value, structure.Value);
        AddSection(sections, DocumentationSectionKind.Remarks, structure.Remarks);
        AddSection(sections, DocumentationSectionKind.Example, structure.Example);

        AddAssociations(associations, DocumentationAssociationKind.TypeParameter, structure.TypeParameters);
        AddAssociations(associations, DocumentationAssociationKind.Parameter, structure.Parameters);
        AddAssociations(associations, DocumentationAssociationKind.Error, structure.Exceptions);
        AddAssociations(associations, DocumentationAssociationKind.Link, structure.See);
        AddAssociations(associations, DocumentationAssociationKind.RelatedLink, structure.SeeAlso);

        return new RavenDocumentation(
            structure.SourceFormat,
            documentation?.Content ?? string.Empty,
            sections.ToImmutable(),
            associations.ToImmutable(),
            structure.InheritDocReference);
    }

    private static void AddSection(
        ImmutableArray<DocumentationSection>.Builder sections,
        DocumentationSectionKind kind,
        string? content)
    {
        if (!string.IsNullOrWhiteSpace(content))
            sections.Add(new DocumentationSection(kind, content.Trim()));
    }

    private static void AddAssociations(
        ImmutableArray<DocumentationAssociation>.Builder associations,
        DocumentationAssociationKind kind,
        ImmutableArray<DocumentationEntry> entries)
    {
        foreach (var entry in entries)
        {
            var isNamedSubject = kind is DocumentationAssociationKind.Parameter or DocumentationAssociationKind.TypeParameter;
            associations.Add(new DocumentationAssociation(
                kind,
                isNamedSubject ? entry.Name : null,
                isNamedSubject ? null : entry.Reference,
                entry.Content));
        }
    }
}
