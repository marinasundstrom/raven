using System;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public sealed class Document
{
    private SyntaxTree? _lazySyntaxTree;
    private SemanticModel? _lazySemanticModel;

    public DocumentId Id { get; }
    public ProjectId ProjectId => Id.ProjectId;
    public DocumentAttributes Attributes { get; }
    public VersionStamp Version { get; }

    public string Name => Attributes.Name;
    public string Text => Attributes.Text;

    public Document(DocumentId id, DocumentAttributes attributes)
    {
        Id = id;
        Attributes = attributes;
        Version = VersionStamp.Create();
    }

    public Document WithAttributes(DocumentAttributes newAttributes)
    {
        return newAttributes.Equals(Attributes) ? this : new Document(Id, newAttributes);
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
}
