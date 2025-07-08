using System;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

public sealed class Document
{
    private readonly Solution _solution;
    private SyntaxTree? _lazySyntaxTree;
    private SemanticModel? _lazySemanticModel;

    public DocumentId Id { get; }
    public ProjectId ProjectId => Id.ProjectId;
    public DocumentAttributes Attributes { get; }
    public VersionStamp Version { get; }

    public string Name => Attributes.Name;
    public string Text => Attributes.Text;

    internal Solution Solution => _solution;
    internal Project Project => _solution.GetProject(ProjectId)!;

    public Document(DocumentId id, string name, string text)
        : this(null!, id, new DocumentAttributes(name, text))
    {

    }

    internal Document(Solution solution, DocumentId id, DocumentAttributes attributes)
    {
        _solution = solution;
        Id = id;
        Attributes = attributes;
        Version = VersionStamp.Create();
    }

    public Document WithAttributes(DocumentAttributes newAttributes)
    {
        return newAttributes.Equals(Attributes) ? this : new Document(_solution, Id, newAttributes);
    }

    public Document WithText(string newText)
    {
        return WithAttributes(Attributes.WithText(newText));
    }

    public Document WithName(string newName)
    {
        return WithAttributes(Attributes.WithName(newName));
    }

    public SyntaxTree GetSyntaxTree()
    {
        if (_lazySyntaxTree is not null)
            return _lazySyntaxTree;

        _lazySyntaxTree = SyntaxTree.ParseText(Attributes.Text);
        return _lazySyntaxTree;
    }

    public SemanticModel GetSemanticModel(Compilation compilation)
    {
        if (_lazySemanticModel is not null)
            return _lazySemanticModel;

        _lazySemanticModel = compilation.GetSemanticModel(GetSyntaxTree());
        return _lazySemanticModel;
    }

    public static Document Create(Project project, DocumentId id, string name, SourceText sourceText)
    {
        return new Document(project.Solution, id, new DocumentAttributes(name, sourceText.ToString()));
    }
}
