using System;

namespace Raven.CodeAnalysis;

public sealed class DocumentAttributes : IEquatable<DocumentAttributes>
{
    public string Name { get; }
    public string Text { get; }

    public DocumentAttributes(string name, string text)
    {
        Name = name;
        Text = text;
    }

    public DocumentAttributes WithName(string name)
    {
        return name == Name ? this : new DocumentAttributes(name, Text);
    }

    public DocumentAttributes WithText(string text)
    {
        return text == Text ? this : new DocumentAttributes(Name, text);
    }

    public bool Equals(DocumentAttributes? other)
    {
        return other != null &&
               Name == other.Name &&
               Text == other.Text;
    }

    public override bool Equals(object? obj) => Equals(obj as DocumentAttributes);

    public override int GetHashCode() => HashCode.Combine(Name, Text);
}
