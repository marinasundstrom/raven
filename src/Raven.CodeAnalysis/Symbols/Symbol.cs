using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Macros;
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
    Event,
    Property,
    Field,
    Error,
    ErrorType,
    TypeParameter,
    Macro
}

/// <summary>
/// Represents a symbol in the Raven compiler's semantic model, such as a namespace, type, method, or variable.
/// </summary>
public interface Symbol : IEquatable<Symbol?>
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
    Symbol? ContainingSymbol { get; }

    /// <summary>
    /// Gets the assembly that contains this symbol, or <c>null</c> if the symbol is not contained in an assembly.
    /// </summary>
    AssemblySymbol? ContainingAssembly { get; }

    /// <summary>
    /// Gets the module that contains this symbol, or <c>null</c> if the symbol is not contained in a module.
    /// </summary>
    ModuleSymbol? ContainingModule { get; }

    /// <summary>
    /// Gets the named type that contains this symbol, or <c>null</c> if the symbol is not contained in a type.
    /// </summary>
    NamedTypeSymbol? ContainingType { get; }

    /// <summary>
    /// Gets the namespace that contains this symbol, or <c>null</c> if the symbol is not contained in a namespace.
    /// </summary>
    NamespaceSymbol? ContainingNamespace { get; }

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
    Symbol UnderlyingSymbol { get; }

    /// <summary>
    /// Gets a value indicating whether this symbol is an alias.
    /// </summary>
    bool IsAlias { get; }

    /// <summary>
    /// Gets the attributes applied to this symbol.
    /// </summary>
    ImmutableArray<AttributeData> GetAttributes();

    /// <summary>
    /// Gets the documentation comment associated with this symbol, if any.
    /// </summary>
    DocumentationComment? GetDocumentationComment() => null;

    bool CanBeReferencedByName => this switch
    {
        NamespaceOrTypeSymbol => true,
        MethodSymbol => true,
        MacroDeclarationSymbol => true,
        EventSymbol => true,
        PropertySymbol property => !property.IsIndexer,
        FieldSymbol => true,
        ParameterSymbol => true,
        LocalSymbol => true,
        LabelSymbol => true,
        _ => false
    };

    /// <summary>
    /// Determines whether this symbol is equal to another symbol using the specified symbol equality comparer.
    /// </summary>
    /// <param name="other">The other symbol to compare with.</param>
    /// <param name="comparer">The comparer to use for symbol equality.</param>
    /// <returns><c>true</c> if the symbols are considered equal; otherwise, <c>false</c>.</returns>
    bool Equals(Symbol? other, SymbolEqualityComparer comparer);

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

public interface NamespaceOrTypeSymbol : Symbol
{
    bool IsNamespace { get; }
    bool IsType { get; }
    ImmutableArray<Symbol> GetMembers();
    ImmutableArray<NamedTypeSymbol> GetTypeMembers() => [.. GetMembers().OfType<NamedTypeSymbol>()];
    ImmutableArray<Symbol> GetMembers(string name);
    ImmutableArray<NamedTypeSymbol> GetTypeMembers(string name) => [.. GetMembers(name).OfType<NamedTypeSymbol>()];
    ImmutableArray<NamedTypeSymbol> GetTypeMembers(string name, int arity) =>
        [.. GetTypeMembers(name).Where(type => type.Arity == arity)];

    TypeSymbol? LookupType(string name);

    bool IsMemberDefined(string name, out Symbol? symbol);
}

public interface NamespaceSymbol : NamespaceOrTypeSymbol
{
    /// <summary>Whether this lookup scope represents logical modules (explicit source or neoCLR target).</summary>
    /// <remarks>A merged lookup scope may include multiple assembly-owned modules. ContainingModule
    /// continues to denote the physical compiler module; this flag does not merge declaration identities.</remarks>
    bool IsModule => false;

    bool IsGlobalNamespace { get; }

    NamespaceSymbol? LookupNamespace(string name);

    string? ToMetadataName();
}

public interface LambdaSymbol : MethodSymbol
{
    TypeSymbol? DelegateType { get; }
}

public interface LabelSymbol : Symbol
{
}

public interface AliasSymbol : Symbol
{

}

public interface MethodSymbol : Symbol
{
    MethodKind MethodKind { get; }
    /// <summary>The explicit whole-signature operand, or null for an ordinary declaration.</summary>
    TypeSymbol? DeclaredFunctionType => null;
    TypeSymbol ReturnType { get; }
    ImmutableArray<ParameterSymbol> Parameters { get; }
    ImmutableArray<AttributeData> GetReturnTypeAttributes();
    bool IsConstructor => MethodKind is MethodKind.Constructor;
    MethodSymbol? OriginalDefinition { get; }
    Symbol? AssociatedSymbol => null;

    int Arity => TypeArguments.Length;

    bool IsAbstract { get; }
    bool IsAsync { get; }
    bool IsCheckedBuiltin { get; }
    bool IsDefinition { get; }
    bool IsExtensionMethod { get; }
    bool IsExtern { get; }
    bool IsUnsafe { get; }
    bool IsGenericMethod { get; }
    bool IsOverride { get; }
    bool IsReadOnly { get; }
    bool IsFinal { get; }
    bool IsVirtual { get; }
    bool IsIterator { get; }
    IteratorMethodKind IteratorKind { get; }
    TypeSymbol? IteratorElementType { get; }

    bool IsLambda => false;

    ImmutableArray<MethodSymbol> ExplicitInterfaceImplementations { get; }

    ImmutableArray<TypeParameterSymbol> TypeParameters { get; }

    ImmutableArray<TypeSymbol> TypeArguments { get; }

    MethodSymbol? ConstructedFrom { get; }

    MethodSymbol Construct(params TypeSymbol[] typeArguments);

    bool SetsRequiredMembers { get; }
}

/// <summary>
/// Represents a Raven compile-time macro.
/// </summary>
public interface MacroSymbol : Symbol
{
    /// <summary>
    /// Gets how this macro is applied to authored Raven syntax.
    /// </summary>
    MacroApplicationKind ApplicationKind { get; }

    /// <summary>
    /// Gets the grammar positions in which this macro can be invoked, or
    /// <see cref="MacroInvocationTargets.None"/> for an attached macro.
    /// </summary>
    MacroInvocationTargets InvocationTargets { get; }

    /// <summary>
    /// Gets the semantic type promised by an expression macro, or
    /// <see langword="null"/> for a category-only expression contract.
    /// </summary>
    TypeSymbol? ExpressionResultType => null;

    /// <summary>
    /// Gets the category of macro represented by this symbol.
    /// </summary>
    MacroKind MacroKind { get; }

    /// <summary>
    /// Gets the declaration targets accepted by this macro, or
    /// <see cref="Macros.MacroTarget.None"/> for a freestanding macro.
    /// </summary>
    MacroTarget Targets { get; }

    /// <summary>
    /// Gets the namespace-qualified language-facing macro name.
    /// </summary>
    string CanonicalName => ContainingNamespace is { IsGlobalNamespace: false } containingNamespace
        ? $"{containingNamespace.ToMetadataName()}.{Name}"
        : Name;

    /// <summary>
    /// Gets the alternate unqualified invocation names exported by the macro.
    /// </summary>
    ImmutableArray<string> Aliases => [];
}

/// <summary>
/// Represents a Raven macro declared with <c>macro</c>.
/// </summary>
/// <remarks>
/// A macro declaration has a callable-shaped source signature, but it is not a
/// CLR method and does not implement <see cref="MethodSymbol"/>.
/// </remarks>
public interface MacroDeclarationSymbol : MacroSymbol
{

    /// <summary>
    /// Gets the canonical nominal type that owns this macro's generic
    /// parameters and expansion entry point.
    /// </summary>
    NamedTypeSymbol DefinitionType { get; }

    /// <summary>
    /// Gets the macro's single designated expansion method.
    /// </summary>
    MethodSymbol ExpandMethod { get; }

    /// <summary>
    /// Gets the source name bound to the current attached target.
    /// </summary>
    string? TargetName { get; }

    /// <summary>
    /// Gets the implicit parameter representing the current attached target.
    /// </summary>
    ParameterSymbol? TargetParameter { get; }

    /// <summary>
    /// Gets the type produced at the macro invocation site.
    /// </summary>
    TypeSymbol ReturnType { get; }

    /// <summary>
    /// Gets the macro declaration parameters.
    /// </summary>
    ImmutableArray<ParameterSymbol> Parameters { get; }

    /// <summary>
    /// Gets how the designated expansion method's parameters are supplied.
    /// </summary>
    ImmutableArray<MacroParameterBinding> ParameterBindings { get; }

    /// <summary>
    /// Gets the macro declaration type parameters.
    /// </summary>
    ImmutableArray<TypeParameterSymbol> TypeParameters { get; }

    /// <summary>
    /// Gets the constructed macro type arguments, or the definition's type
    /// parameters when this is an unconstructed macro.
    /// </summary>
    ImmutableArray<TypeSymbol> TypeArguments => DefinitionType.TypeArguments;

    /// <summary>
    /// Gets the generic arity of the macro declaration.
    /// </summary>
    int Arity => TypeParameters.Length;

    /// <summary>
    /// Gets whether this macro declaration is generic.
    /// </summary>
    bool IsGenericMacro => Arity > 0;

    /// <summary>
    /// Gets the unconstructed macro declaration.
    /// </summary>
    MacroDeclarationSymbol OriginalDefinition => this;

    /// <summary>
    /// Constructs this macro declaration with explicit symbolic type
    /// arguments.
    /// </summary>
    MacroDeclarationSymbol Construct(params TypeSymbol[] typeArguments);
}

public enum MethodKind
{
    LambdaMethod,
    Constructor,
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
    InitOnly,
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
    Enumerator,
    AsyncEnumerable,
    AsyncEnumerator
}

public interface ParameterSymbol : Symbol
{
    TypeSymbol Type { get; }

    /// <summary>
    /// Gets whether the parameter has a compiler-projected metadata name rather
    /// than a name written in Raven source.
    /// </summary>
    bool HasImplicitName => false;

    /// <summary>
    /// Gets the binding structure presented to callers, or null for an ordinary
    /// named parameter or unavailable metadata. Imported patterns are detached
    /// syntax for presentation, not declarations in the consuming compilation.
    /// </summary>
    PatternSyntax? BindingPattern => null;

    /// <summary>
    /// Gets how this parameter is supplied when it belongs to a macro
    /// function, or <see cref="MacroParameterRole.None"/> otherwise.
    /// </summary>
    MacroParameterRole MacroRole => MacroParameterRole.None;

    bool IsVarParams { get; }

    RefKind RefKind { get; }

    ScopedKind ScopedKind => ScopedKind.None;

    bool IsMutable { get; }

    bool IsOptional => HasExplicitDefaultValue;

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

public enum ScopedKind
{
    None,
    ScopedRef,
    ScopedValue
}


public interface FieldSymbol : Symbol
{
    TypeSymbol Type { get; }
    RefKind RefKind => RefKind.None;

    bool IsConst { get; }

    bool IsReadOnly { get; }

    bool IsRequired { get; }

    object? GetConstantValue();

    /// <summary>
    /// For auto-property or event backing fields: the owning property or event symbol.
    /// Returns null for regular fields.
    /// </summary>
    Symbol? AssociatedSymbol => null;
}

public interface PropertySymbol : Symbol
{
    PropertySymbol? OriginalDefinition => null;
    TypeSymbol Type { get; }
    MethodSymbol? GetMethod { get; }
    MethodSymbol? SetMethod { get; }
    bool IsMutable => SetMethod is { MethodKind: MethodKind.PropertySet };
    bool IsIndexer { get; }
    bool IsRequired { get; }
    /// <summary>Gets ordered index parameters, excluding a setter's value parameter; empty for ordinary properties.</summary>
    ImmutableArray<ParameterSymbol> Parameters => !IsIndexer ? [] : GetMethod?.Parameters ??
        (SetMethod is { Parameters: var parameters } && !parameters.IsEmpty ? parameters.RemoveAt(parameters.Length - 1) : []);
    ImmutableArray<PropertySymbol> ExplicitInterfaceImplementations => [];
    bool IsExtensionProperty => false;
}

public interface EventSymbol : Symbol
{
    TypeSymbol Type { get; }
    MethodSymbol? AddMethod { get; }
    MethodSymbol? RemoveMethod { get; }
    ImmutableArray<EventSymbol> ExplicitInterfaceImplementations => [];
}

public interface TypeSymbol : NamespaceOrTypeSymbol
{
    NamedTypeSymbol? BaseType { get; }
    TypeSymbol? OriginalDefinition { get; }

    SpecialType SpecialType { get; }

    TypeKind TypeKind { get; }

    ImmutableArray<NamedTypeSymbol> Interfaces { get; }

    ImmutableArray<NamedTypeSymbol> AllInterfaces { get; }

    public string ToFullyQualifiedMetadataName()
    {
        string typeName;

        if (this is NamedTypeSymbol named)
        {
            var segments = new Stack<string>();

            for (NamedTypeSymbol? current = named; current is not null; current = GetMetadataContainingType(current))
            {
                var segment = current.MetadataName ?? current.Name ?? string.Empty;

                // Keep only the local metadata segment when symbols expose fully-qualified metadata names.
                var plusIndex = segment.LastIndexOf('+');
                if (plusIndex >= 0 && plusIndex + 1 < segment.Length)
                    segment = segment.Substring(plusIndex + 1);

                var dotIndex = segment.LastIndexOf('.');
                if (dotIndex >= 0 && dotIndex + 1 < segment.Length)
                    segment = segment.Substring(dotIndex + 1);

                segments.Push(segment);
            }

            typeName = segments.Count > 0
                ? string.Join("+", segments)
                : MetadataName;

            static NamedTypeSymbol? GetMetadataContainingType(NamedTypeSymbol type)
                => type is UnionCaseTypeSymbol { IsUnionCase: true } unionCase
                    ? unionCase.MetadataContainingType
                    : type.ContainingType;
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

    bool IsInterface => false;

    bool IsTupleType => TypeKind == TypeKind.Tuple || this is TupleTypeSymbol;

    bool IsNullable => TypeKind == TypeKind.Nullable;

    bool IsUnion
    {
        get
        {
            if (this is UnionSymbol)
                return true;

            return GetAttributes().Any(static attribute =>
                attribute.AttributeClass?.ToFullyQualifiedMetadataName() is
                    "System.Runtime.CompilerServices.UnionAttribute" or
                    "System.Runtime.CompilerServices.DiscriminatedUnionAttribute");
        }
    }

    bool IsUnionCase
    {
        get
        {
            if (this is UnionCaseTypeSymbol)
                return true;

            return false;
        }
    }

    NamedTypeSymbol? UnderlyingUnionType
    {
        get
        {
            if (this is UnionSymbol union)
                return union;

            if (this is UnionCaseTypeSymbol caseSymbol)
                return caseSymbol.Union;

            return null;
        }
    }
}

/// <summary>A semantic all-of type expression without an implied runtime representation.</summary>
public interface IntersectionTypeSymbol : TypeSymbol
{
    /// <summary>Gets normalized bounds in presentation order; identity ignores this order.</summary>
    ImmutableArray<TypeSymbol> ConstituentTypes { get; }
}

/// <summary>An experimental any-of type expression, distinct from nominal union carriers.</summary>
public interface StructuralUnionTypeSymbol : TypeSymbol
{
    /// <summary>Gets alternatives in presentation order; identity ignores ordering.</summary>
    ImmutableArray<TypeSymbol> ConstituentTypes { get; }
}

/// <summary>A dependent projection of a structural function's result type.</summary>
public interface FunctionReturnTypeSymbol : TypeSymbol
{
    /// <summary>The function-constrained operand whose result will be substituted.</summary>
    TypeSymbol FunctionType { get; }
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
    Address,
    Struct,
    TypeParameter,
    FunctionPointer,
    Tuple,
    Nullable,
    Null,
    Unit,
    Intersection,
    StructuralUnion,
    FunctionReturn
}

[Flags]
public enum TypeParameterConstraintKind
{
    None = 0,
    ReferenceType = 1 << 0,
    ValueType = 1 << 1,
    TypeConstraint = 1 << 2,
    NotNull = 1 << 3,
    Constructor = 1 << 4,
    AllowByRefLike = 1 << 5,
    /// <summary>A native neoCLR structural function signature; nominal delegates do not satisfy this constraint.</summary>
    FunctionType = 1 << 6,
}

public enum VarianceKind
{
    None,
    Out,
    In
}

public enum TypeParameterOwnerKind
{
    Type,
    Method
}

public interface NamedTypeSymbol : TypeSymbol
{
    int Arity { get; }
    ImmutableArray<MethodSymbol> Constructors { get; }
    ImmutableArray<MethodSymbol> InstanceConstructors { get; }
    MethodSymbol? StaticConstructor { get; }

    TypeSymbol? EnumUnderlyingType => null;
    /// <summary>
    /// Gets the underlying <c>System.ValueTuple</c> type for a tuple projection,
    /// or <see langword="null"/> when this symbol is not a tuple type.
    /// </summary>
    NamedTypeSymbol? UnderlyingTupleType { get; }
    ImmutableArray<FieldSymbol> TupleElements { get; }
    ImmutableArray<TypeSymbol> TypeArguments { get; }
    ImmutableArray<TypeParameterSymbol> TypeParameters { get; }
    TypeSymbol? ConstructedFrom { get; }
    bool IsAbstract { get; }
    bool IsClosed { get; }
    bool IsOpen => !IsClosed;
    bool IsRefLikeType => false;
    bool IsReadOnly => false;
    bool IsSealedHierarchy => false;
    ImmutableArray<NamedTypeSymbol> PermittedDirectSubtypes => ImmutableArray<NamedTypeSymbol>.Empty;

    bool IsGenericType { get; }
    bool IsUnboundGenericType { get; }

    TypeSymbol Construct(params TypeSymbol[] typeArguments);
}

public interface ArrayTypeSymbol : TypeSymbol
{
    TypeSymbol ElementType { get; }

    public int Rank { get; }

    public bool IsFixedArray { get; }

    public int? FixedLength { get; }
}

public interface PointerTypeSymbol : TypeSymbol
{
    TypeSymbol PointedAtType { get; }
}

public interface AddressTypeSymbol : TypeSymbol
{
    TypeSymbol ReferencedType { get; }
}

public interface TupleTypeSymbol : NamedTypeSymbol
{

}

public interface UnionSymbol : NamedTypeSymbol
{
    /// <summary>
    /// Gets the types of all variants declared by the union, whether they come
    /// from parenthesized types or <c>case</c> declarations.
    /// </summary>
    ImmutableArray<TypeSymbol> Variants { get; }

    /// <summary>
    /// Gets the generated variant types introduced specifically by
    /// <c>case</c> declarations.
    /// </summary>
    ImmutableArray<UnionCaseTypeSymbol> DeclaredCaseTypes { get; }

    ImmutableArray<TypeSymbol> MemberTypes { get; }

    bool ContentMayBeNull { get; }

    FieldSymbol DiscriminatorField { get; }

    FieldSymbol PayloadField { get; }
}

public interface UnionCaseTypeSymbol : NamedTypeSymbol
{
    UnionSymbol Union { get; }

    NamedTypeSymbol MetadataContainingType { get; }

    ImmutableArray<ParameterSymbol> ConstructorParameters { get; }

    int Ordinal { get; }
}

public interface TypeParameterSymbol : TypeSymbol
{
    int Ordinal { get; }

    TypeParameterOwnerKind OwnerKind { get; }

    NamedTypeSymbol? DeclaringTypeParameterOwner { get; }

    MethodSymbol? DeclaringMethodParameterOwner { get; }

    TypeParameterConstraintKind ConstraintKind { get; }

    ImmutableArray<TypeSymbol> ConstraintTypes { get; }

    VarianceKind Variance { get; }
}

public interface LocalSymbol : Symbol
{
    TypeSymbol Type { get; }

    ScopedKind ScopedKind => ScopedKind.None;

    bool IsMutable { get; }

    bool IsConst { get; }

    object? ConstantValue { get; }
}

public interface ErrorSymbol : Symbol
{
}

public interface ErrorTypeSymbol : NamedTypeSymbol
{
}

public interface AssemblySymbol : Symbol
{
    NamespaceSymbol GlobalNamespace { get; }

    IEnumerable<ModuleSymbol> Modules { get; }

    NamedTypeSymbol? GetTypeByMetadataName(string fullyQualifiedMetadataName);

    //Symbol? ResolveMetadataMember(NamespaceSymbol namespaceSymbol, string name);
}

public interface ModuleSymbol : Symbol
{
    NamespaceSymbol GlobalNamespace { get; }

    //ImmutableArray<AssemlyIdentity> ReferencedAssembly { get; }

    ImmutableArray<AssemblySymbol> ReferencedAssemblySymbols { get; }

    NamespaceSymbol? GetModuleNamespace(NamespaceSymbol namespaceSymbol);
}
