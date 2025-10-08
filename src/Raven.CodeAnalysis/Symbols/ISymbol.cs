using System;
using System.Collections.Generic;
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
    Label,
    Property,
    Field,
    ErrorType,
    TypeParameter
}

/// <summary>
/// Represents a symbol in the Raven compiler's semantic model, such as a namespace, type, method, or variable.
/// </summary>
public interface ISymbol : IEquatable<ISymbol?>
{
    /// <summary>
    /// Gets the kind of symbol (e.g., namespace, type, method).
    /// </summary>
    SymbolKind Kind { get; }

    /// <summary>
    /// Gets the name of the symbol.
    /// </summary>
    string Name { get; }

    /// <summary>
    /// Gets the name used in metadata to identify the symbol.
    /// </summary>
    string MetadataName { get; }

    /// <summary>
    /// Gets the symbol that contains this symbol. May be <c>null</c> for root symbols.
    /// </summary>
    ISymbol? ContainingSymbol { get; }

    /// <summary>
    /// Gets the assembly that contains this symbol, or <c>null</c> if the symbol is not contained in an assembly.
    /// </summary>
    IAssemblySymbol? ContainingAssembly { get; }

    /// <summary>
    /// Gets the module that contains this symbol, or <c>null</c> if the symbol is not contained in a module.
    /// </summary>
    IModuleSymbol? ContainingModule { get; }

    /// <summary>
    /// Gets the named type that contains this symbol, or <c>null</c> if the symbol is not contained in a type.
    /// </summary>
    INamedTypeSymbol? ContainingType { get; }

    /// <summary>
    /// Gets the namespace that contains this symbol, or <c>null</c> if the symbol is not contained in a namespace.
    /// </summary>
    INamespaceSymbol? ContainingNamespace { get; }

    /// <summary>
    /// Gets the source or metadata locations associated with this symbol.
    /// </summary>
    ImmutableArray<Location> Locations { get; }

    /// <summary>
    /// Gets the declared accessibility (e.g., public, internal, private) of this symbol.
    /// </summary>
    Accessibility DeclaredAccessibility { get; }

    /// <summary>
    /// Gets the syntax references used to declare this symbol.
    /// </summary>
    ImmutableArray<SyntaxReference> DeclaringSyntaxReferences { get; }

    /// <summary>
    /// Gets a value indicating whether the symbol was implicitly declared by the compiler.
    /// </summary>
    bool IsImplicitlyDeclared { get; }

    /// <summary>
    /// Gets a value indicating whether the symbol is static.
    /// </summary>
    bool IsStatic { get; }

    /// <summary>
    /// Gets the symbol that this symbol ultimately represents. For non-alias symbols, this returns <c>this</c>.
    /// </summary>
    ISymbol UnderlyingSymbol { get; }

    /// <summary>
    /// Gets a value indicating whether this symbol is an alias.
    /// </summary>
    bool IsAlias { get; }

    /// <summary>
    /// Gets the attributes applied to this symbol.
    /// </summary>
    ImmutableArray<AttributeData> GetAttributes();

    bool CanBeReferencedByName => this switch
    {
        INamespaceOrTypeSymbol => true,
        IMethodSymbol => true,
        IPropertySymbol => true,
        IFieldSymbol => true,
        IParameterSymbol => true,
        ILocalSymbol => true,
        ILabelSymbol => true,
        _ => false
    };

    /// <summary>
    /// Determines whether this symbol is equal to another symbol using the specified symbol equality comparer.
    /// </summary>
    /// <param name="other">The other symbol to compare with.</param>
    /// <param name="comparer">The comparer to use for symbol equality.</param>
    /// <returns><c>true</c> if the symbols are considered equal; otherwise, <c>false</c>.</returns>
    bool Equals(ISymbol? other, SymbolEqualityComparer comparer);

    /// <summary>
    /// Accepts a symbol visitor to perform an operation on this symbol.
    /// </summary>
    /// <param name="visitor">The visitor to accept.</param>
    void Accept(SymbolVisitor visitor);

    /// <summary>
    /// Accepts a symbol visitor to perform an operation on this symbol and returns a result.
    /// </summary>
    /// <typeparam name="TResult">The type of result returned by the visitor.</typeparam>
    /// <param name="visitor">The visitor to accept.</param>
    /// <returns>The result produced by the visitor.</returns>
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
        Span = node.EffectiveSpan;
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

public interface ILambdaSymbol : IMethodSymbol
{
    ITypeSymbol? DelegateType { get; }
}

public interface ILabelSymbol : ISymbol
{
}

public interface IAliasSymbol : ISymbol
{

}

public interface IMethodSymbol : ISymbol
{
    MethodKind MethodKind { get; }
    ITypeSymbol ReturnType { get; }
    ImmutableArray<IParameterSymbol> Parameters { get; }
    ImmutableArray<AttributeData> GetReturnTypeAttributes();
    bool IsConstructor => MethodKind is MethodKind.Constructor;
    bool IsNamedConstructor => false;
    IMethodSymbol? OriginalDefinition { get; }

    bool IsAbstract { get; }
    bool IsAsync { get; }
    bool IsCheckedBuiltin { get; }
    bool IsDefinition { get; }
    bool IsExtensionMethod { get; }
    bool IsExtern { get; }
    bool IsGenericMethod { get; }
    bool IsOverride { get; }
    bool IsReadOnly { get; }
    bool IsSealed { get; }
    bool IsVirtual { get; }
    bool IsIterator { get; }
    IteratorMethodKind IteratorKind { get; }
    ITypeSymbol? IteratorElementType { get; }

    ImmutableArray<IMethodSymbol> ExplicitInterfaceImplementations { get; }

    ImmutableArray<ITypeParameterSymbol> TypeParameters { get; }

    ImmutableArray<ITypeSymbol> TypeArguments { get; }

    IMethodSymbol? ConstructedFrom { get; }

    IMethodSymbol Construct(params ITypeSymbol[] typeArguments);

}

public enum MethodKind
{
    LambdaMethod,
    Constructor,
    NamedConstructor,
    Conversion,
    Destructor,
    EventAdd,
    EventRaise,
    EventRemove,
    ExplicitInterfaceImplementation,
    UserDefinedOperator,
    Ordinary,
    PropertyGet,
    PropertySet,
    ReducedExtension,
    StaticConstructor,
    BuiltinOperator,
    Function,
    FunctionPointerSignature
}

public enum IteratorMethodKind
{
    None,
    Enumerable,
    Enumerator
}

public interface IParameterSymbol : ISymbol
{
    ITypeSymbol Type { get; }

    bool IsParams { get; }

    RefKind RefKind { get; }

    bool HasExplicitDefaultValue { get; }

    object? ExplicitDefaultValue { get; }
}


public enum RefKind
{
    None,
    Ref,
    Out,
    In,
    RefReadOnly,
    RefReadOnlyParameter
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
    ITypeSymbol? OriginalDefinition { get; }

    SpecialType SpecialType { get; }

    TypeKind TypeKind { get; }

    ImmutableArray<INamedTypeSymbol> Interfaces { get; }

    ImmutableArray<INamedTypeSymbol> AllInterfaces { get; }

    public string ToFullyQualifiedMetadataName()
    {
        string typeName;

        if (this is INamedTypeSymbol named)
        {
            var segments = new Stack<string>();

            for (INamedTypeSymbol? current = named; current is not null; current = current.ContainingType)
                segments.Push(current.MetadataName);

            typeName = segments.Count > 0
                ? string.Join("+", segments)
                : MetadataName;
        }
        else
        {
            typeName = MetadataName;
        }

        var containingNamespace = ContainingNamespace;

        if (containingNamespace is null || containingNamespace.IsGlobalNamespace)
            return typeName;

        var namespaceName = containingNamespace.ToMetadataName();

        return string.IsNullOrEmpty(namespaceName)
            ? typeName
            : $"{namespaceName}.{typeName}";
    }

    bool IsReferenceType => !IsValueType;

    bool IsValueType => false;

    //bool IsInterface => false;

    bool IsTupleType => MetadataName.Contains("System.ValueTuple");

    bool IsUnion => TypeKind == TypeKind.Union;
}

public enum TypeKind
{
    Unknown,
    Array,
    Class,
    Delegate,
    Enum,
    Error,
    Interface,
    Pointer,
    Struct,
    TypeParameter,
    FunctionPointer,
    Union,
    Tuple,
    Nullable,
    Null,
    Unit
}

[Flags]
public enum TypeParameterConstraintKind
{
    None = 0,
    ReferenceType = 1 << 0,
    ValueType = 1 << 1,
    TypeConstraint = 1 << 2,
}

public enum VarianceKind
{
    None,
    Out,
    In
}

public interface INamedTypeSymbol : ITypeSymbol
{
    int Arity { get; }
    ImmutableArray<IMethodSymbol> Constructors { get; }
    IMethodSymbol? StaticConstructor { get; }
    INamedTypeSymbol UnderlyingTupleType { get; }
    ImmutableArray<IFieldSymbol> TupleElements { get; }
    ImmutableArray<ITypeSymbol> TypeArguments { get; }
    ImmutableArray<ITypeParameterSymbol> TypeParameters { get; }
    ITypeSymbol? ConstructedFrom { get; }
    bool IsAbstract { get; }
    bool IsSealed { get; }
    bool IsGenericType { get; }
    bool IsUnboundGenericType { get; }

    ITypeSymbol Construct(params ITypeSymbol[] typeArguments);
}

public interface IArrayTypeSymbol : ITypeSymbol
{
    ITypeSymbol ElementType { get; }

    public int Rank { get; }
}

public interface ITupleTypeSymbol : INamedTypeSymbol
{

}

public interface IUnionTypeSymbol : ITypeSymbol
{
    IEnumerable<ITypeSymbol> Types { get; }

    /// <summary>
    /// Gets the CLR type that originally declared the union, if any.
    /// This is used when reading metadata from external assemblies so that
    /// the runtime representation can remain compatible with the original
    /// signature (e.g. object vs ValueType).
    /// </summary>
    ITypeSymbol? DeclaredUnderlyingType { get; }
}

public interface ITypeParameterSymbol : ITypeSymbol
{
    TypeParameterConstraintKind ConstraintKind { get; }

    ImmutableArray<ITypeSymbol> ConstraintTypes { get; }

    VarianceKind Variance { get; }
}

public interface ILocalSymbol : ISymbol
{
    ITypeSymbol Type { get; }

    bool IsMutable { get; }
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
