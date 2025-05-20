using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public enum SymbolKind
{
    Assembly,
    Module,
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

    string MetadataName { get; }

    public ISymbol? ContainingSymbol { get; }

    IAssemblySymbol? ContainingAssembly { get; }

    IModuleSymbol? ContainingModule { get; }

    INamedTypeSymbol? ContainingType { get; }

    INamespaceSymbol? ContainingNamespace { get; }

    ImmutableArray<Location> Locations { get; }

    Accessibility DeclaredAccessibility { get; }

    ImmutableArray<SyntaxReference> DeclaringSyntaxReferences { get; }

    bool IsImplicitlyDeclared { get; }

    bool IsStatic { get; }

    bool Equals(ISymbol? other, SymbolEqualityComparer comparer);

    void Accept(SymbolVisitor visitor);

    TResult Accept<TResult>(SymbolVisitor<TResult> visitor);
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

    ITypeSymbol? LookupType(string name);

    bool IsMemberDefined(string name, out ISymbol? symbol);
}

public interface INamespaceSymbol : INamespaceOrTypeSymbol
{
    bool IsGlobalNamespace { get; }

    INamespaceSymbol? LookupNamespace(string name);

    string? ToMetadataName();
}

public interface IMethodSymbol : ISymbol
{
    ITypeSymbol ReturnType { get; }
    ImmutableArray<IParameterSymbol> Parameters { get; }
    bool IsConstructor { get; }
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
    bool IsIndexer { get; }
}

public interface ITypeSymbol : INamespaceOrTypeSymbol
{
    INamedTypeSymbol? BaseType { get; }
    SpecialType SpecialType { get; }
    bool IsValueType { get; }
    bool IsArray { get; }

    public string ToFullyQualifiedMetadataName() => ContainingNamespace is null ? Name : $"{ContainingNamespace.ToMetadataName()}.{Name}";
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

    public int Rank { get; }
}

public interface ITypeParameterSymbol : ISymbol
{
}

public interface ILocalSymbol : ISymbol
{
    ITypeSymbol Type { get; }

    bool IsReadOnly { get; }
}

public interface IErrorTypeSymbol : INamedTypeSymbol
{
}

public interface IAssemblySymbol : ISymbol
{
    INamespaceSymbol GlobalNamespace { get; }

    IEnumerable<IModuleSymbol> Modules { get; }

    INamedTypeSymbol? GetTypeByMetadataName(string fullyQualifiedMetadataName);

    //ISymbol? ResolveMetadataMember(INamespaceSymbol namespaceSymbol, string name);
}

public interface IModuleSymbol : ISymbol
{
    INamespaceSymbol GlobalNamespace { get; }

    //ImmutableArray<AssemlyIdentity> ReferencedAssembly { get; }

    ImmutableArray<IAssemblySymbol> ReferencedAssemblySymbols { get; }

    INamespaceSymbol? GetModuleNamespace(INamespaceSymbol namespaceSymbol);
}