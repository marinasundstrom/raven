using System.Diagnostics;

namespace Raven.CodeAnalysis.Syntax;

/// <summary>
/// Represents a metadata annotation that can be attached to syntax nodes, tokens, or trivia.
/// These are ignored for equality and hashing of the tree structure.
/// </summary>
[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public sealed class SyntaxAnnotation : IEquatable<SyntaxAnnotation>
{
    public string Kind { get; }
    public string? Data { get; }

    public SyntaxAnnotation(string kind)
        : this(kind, null)
    {
    }

    public SyntaxAnnotation(string kind, string? data)
    {
        Kind = kind ?? throw new ArgumentNullException(nameof(kind));
        Data = data;
    }

    public override bool Equals(object? obj)
        => Equals(obj as SyntaxAnnotation);

    public bool Equals(SyntaxAnnotation? other)
        => other is not null &&
           Kind == other.Kind &&
           Data == other.Data;

    public override int GetHashCode()
    {
        unchecked
        {
            return (Kind.GetHashCode() * 397) ^ (Data?.GetHashCode() ?? 0);
        }
    }

    public static bool operator ==(SyntaxAnnotation? left, SyntaxAnnotation? right)
        => Equals(left, right);

    public static bool operator !=(SyntaxAnnotation? left, SyntaxAnnotation? right)
        => !Equals(left, right);

    private string GetDebuggerDisplay()
        => Data is null ? Kind : $"{Kind}: {Data}";
}