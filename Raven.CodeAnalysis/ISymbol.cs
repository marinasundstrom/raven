using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public enum SymbolKind
{
    Namespace,
    Type,
    Method,
    Parameter,
    Local,
    Property,
    Field
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
    
    bool IsImplicitlyDeclared { get; }
    
    string ToDisplayString();
    
    bool Equals(ISymbol? other, SymbolEqualityComparer comparer);
}

public class SyntaxReference
{
    public SyntaxReference(SyntaxTree syntaxTree, TextSpan span)
    {
        SyntaxTree = syntaxTree;
        Span = span;
    }

    public SyntaxTree SyntaxTree { get; }

    public TextSpan Span { get; }

    public SyntaxNode GetSyntax(CancellationToken cancellationToken = default)
    {
        return SyntaxTree.GetNodeForSpan(Span)!;
    }
    
    /*
    public virtual Task<SyntaxNode> GetSyntaxAsync(CancellationToken cancellationToken = default)
    {
        return Task.FromResult(this.GetSyntax(cancellationToken));
    } 
    */

    internal Location GetLocation()
    {
        return this.SyntaxTree.GetLocation(this.Span);
    }
}

public interface INamespaceOrTypeSymbol : ISymbol
{
    bool IsNamespace { get;  }
    
    bool IsType { get;  }

    ImmutableArray<ISymbol> GetMembers();
    
    ImmutableArray<ISymbol> GetMembers(string name);
}

public interface INamespaceSymbol : INamespaceOrTypeSymbol
{
    
}

public interface IMethodSymbol : ISymbol
{
    ITypeSymbol ReturnType { get; }
    ImmutableArray<IParameterSymbol> Parameters { get; set; }
}

public interface IParameterSymbol : ISymbol
{
    ITypeSymbol Type { get; }
}


public interface IFieldSymbol : ISymbol
{
    ITypeSymbol Type { get; }
}

public interface IPropertySymbol : ISymbol
{
    ITypeSymbol Type { get; }
    
    IMethodSymbol? GetMethod { get; }
    
    IMethodSymbol? SetMethod { get; }
}

public interface ITypeSymbol : INamespaceOrTypeSymbol
{
    
}

public interface INamedTypeSymbol : ITypeSymbol
{
    
}

public interface ILocalSymbol : ISymbol
{
    ITypeSymbol Type { get; }
}