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
    
    Accessibility DeclaredAccessibility { get; }

    ImmutableArray<SyntaxReference> DeclaringSyntaxReferences { get; }
    
    bool IsImplicitlyDeclared { get; }
    
    bool Equals(ISymbol? other, SymbolEqualityComparer comparer);

    string ToDisplayString(SymbolDisplayFormat format = default);
}

public enum Accessibility
{
    NotApplicable = 0,
    Private = 1,
    ProtectedAndFriend = 2,
    ProtectedAndInternal = 2,
    ProtectedAndProtected = 3,
    Friend = 4,
    Internal = 4,
    ProtectedOrFriend = 5,
    ProtectedOrInternal = 5,
    Public = 6,
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
    bool IsGlobalNamespace { get; }
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
    ImmutableArray<IMethodSymbol> Constructors { get; }
    IMethodSymbol? StaticConstructor { get; }
    public ImmutableArray<ITypeSymbol> TypeArguments { get; }
    public ImmutableArray<ITypeParameterSymbol> TypeParameters { get; }
}

public interface ITypeParameterSymbol: ISymbol
{
}

public interface ILocalSymbol : ISymbol
{
    ITypeSymbol Type { get; }
}