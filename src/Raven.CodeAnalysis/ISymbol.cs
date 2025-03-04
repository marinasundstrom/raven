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
    Field,
    ErrorType
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

    bool IsStatic { get; }

    bool Equals(ISymbol? other, SymbolEqualityComparer comparer);

    string ToDisplayString(SymbolDisplayFormat format = default);
}

public enum Accessibility
{
    NotApplicable = 0,
    Private = 1,
    ProtectedAndInternal = 2,
    ProtectedAndProtected = 3,
    Internal = 4,
    ProtectedOrInternal = 5,
    Public = 6,
}

public class SyntaxReference
{
    private readonly SyntaxNode? _node;

    // WORKAROUND: SyntaxReference should not store node
    public SyntaxReference(SyntaxTree syntaxTree, SyntaxNode node)
    {
        SyntaxTree = syntaxTree;
        Span = node.Span;
        _node = node;
    }

    public SyntaxReference(SyntaxTree syntaxTree, TextSpan span)
    {
        SyntaxTree = syntaxTree;
        Span = span;
    }

    public SyntaxTree SyntaxTree { get; }

    public TextSpan Span { get; }

    public SyntaxNode GetSyntax(CancellationToken cancellationToken = default)
    {
        // WORKAROUND: SyntaxReference should not store node
        return _node ?? SyntaxTree.GetNodeForSpan(Span)!;

        //return SyntaxTree.GetRoot(cancellationToken).FindNode(Span, getInnermostNodeForTie: true);

        /*
        var root = SyntaxTree.GetRoot(cancellationToken);

        var candidate = root.FindNode(Span, getInnermostNodeForTie: true);

        // Now walk up the ancestor chain to see all nodes with the same span
        var allMatches = candidate
            .AncestorNodesAndSelf()
            .Where(n => n.Span == Span).ToArray();

        return allMatches.First();
        */
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
    bool IsNamespace { get; }
    bool IsType { get; }
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
    ImmutableArray<IParameterSymbol> Parameters { get; }
}

public interface IParameterSymbol : ISymbol
{
    ITypeSymbol Type { get; }
}


public interface IFieldSymbol : ISymbol
{
    ITypeSymbol Type { get; }

    bool IsLiteral { get; }

    object? GetConstantValue();
}

public interface IPropertySymbol : ISymbol
{
    ITypeSymbol Type { get; }
    IMethodSymbol? GetMethod { get; }
    IMethodSymbol? SetMethod { get; }
}

public interface ITypeSymbol : INamespaceOrTypeSymbol
{
    INamedTypeSymbol? BaseType { get; }
    SpecialType SpecialType { get; }
    bool IsValueType { get; }
}

public interface INamedTypeSymbol : ITypeSymbol
{
    ImmutableArray<IMethodSymbol> Constructors { get; }
    IMethodSymbol? StaticConstructor { get; }
    ImmutableArray<ITypeSymbol> TypeArguments { get; }
    ImmutableArray<ITypeParameterSymbol> TypeParameters { get; }
}

public interface IArrayTypeSymbol : ITypeSymbol
{
    ITypeSymbol ElementType { get; }
}

public interface ITypeParameterSymbol : ISymbol
{
}

public interface ILocalSymbol : ISymbol
{
    ITypeSymbol Type { get; }

    bool IsReadOnly { get; }
}