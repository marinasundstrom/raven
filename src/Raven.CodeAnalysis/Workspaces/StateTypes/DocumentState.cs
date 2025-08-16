using System;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using static Raven.CodeAnalysis.DocumentInfo;

namespace Raven.CodeAnalysis;

/// <summary>
/// State object for a <see cref="Document"/>.  Tracks the underlying text and
/// parse options and provides helpers to produce updated states.
/// </summary>
sealed class DocumentState : TextDocumentState
{
    public ParseOptions ParseOptions { get; }

    public DocumentState(DocumentAttributes attribute, ITextAndVersionSource textSource, ParseOptions parseOptions)
        : base(attribute, textSource)
    {
        ParseOptions = parseOptions;
    }

    public DocumentState WithText(SourceText newText)
    {
        if (newText == null) throw new ArgumentNullException(nameof(newText));
        var newAttributes = Attributes.WithText(newText);
        return new DocumentState(newAttributes, TextAndVersionSource, ParseOptions);
    }
}
