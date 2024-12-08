using System.Collections.Immutable;
using System.Diagnostics;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public enum SymbolKind
{
    Local
}

public interface ISymbol : IEquatable<ISymbol?>
{
    SymbolKind Kind { get; }

    string Name { get; }

    public ISymbol? ContainingSymbol { get; }

    INamedTypeSymbol? ContainingType { get; }

    INamespaceSymbol? ContainingNamespace { get; }

    ImmutableArray<Location> Locations { get; }

    ImmutableArray<SyntaxReference> DeclaringSyntaxReferences { get; }
}

public abstract class SyntaxReference
{
    public abstract SyntaxTree SyntaxTree { get; }

    public abstract TextSpan Span { get; }

    public abstract SyntaxNode GetSyntax(CancellationToken cancellationToken = default);

    public virtual Task<SyntaxNode> GetSyntaxAsync(CancellationToken cancellationToken = default)
    {
        return Task.FromResult(this.GetSyntax(cancellationToken));
    }

    internal Location GetLocation()
    {
        return this.SyntaxTree.GetLocation(this.Span);
    }
}

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public abstract class Location
{
    public string GetDebuggerDisplay() => "Foo";
}

public interface INamespaceSymbol : ISymbol
{

}


public interface ITypeSymbol : ISymbol
{

}

public interface INamedTypeSymbol : ITypeSymbol
{

}


internal interface ILocalSymbol : ISymbol
{
    string Name { get; }

    ITypeSymbol Type { get; }
}