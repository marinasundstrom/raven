using System.Collections.Immutable;

using Raven.CodeAnalysis.Documentation;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class ProjectedMethodSymbol : SymbolBase, MethodSymbol
{
    internal ProjectedMethodSymbol(NamedTypeSymbol receiverType, MethodSymbol adapterMethod)
        : base(
            receiverType,
            receiverType,
            receiverType.ContainingNamespace,
            [],
            [],
            adapterMethod.DeclaredAccessibility,
            addAsMember: false)
    {
        AdapterMethod = adapterMethod;
    }

    internal MethodSymbol AdapterMethod { get; }

    public override SymbolKind Kind => SymbolKind.Method;
    public override string Name => AdapterMethod.Name;
    public override string MetadataName => AdapterMethod.MetadataName;
    public override AssemblySymbol? ContainingAssembly => ContainingType?.ContainingAssembly;
    public override ModuleSymbol? ContainingModule => ContainingType?.ContainingModule;
    public override bool IsImplicitlyDeclared => true;
    public override bool IsStatic => AdapterMethod.ExtensionMemberKind == ExtensionMemberKind.Static;
    public override Symbol UnderlyingSymbol => AdapterMethod;
    public override DocumentationComment? GetDocumentationComment() => AdapterMethod.GetDocumentationComment();
    public MethodKind MethodKind => AdapterMethod.MethodKind;
    public TypeSymbol ReturnType => AdapterMethod.ReturnType;
    public ImmutableArray<ParameterSymbol> Parameters => AdapterMethod.ExtensionMemberKind == ExtensionMemberKind.Instance
        ? AdapterMethod.Parameters.RemoveAt(0)
        : AdapterMethod.Parameters;
    public MethodSymbol? OriginalDefinition => this;
    public bool IsAbstract => false;
    public bool IsAsync => false;
    public bool IsCheckedBuiltin => false;
    public bool IsDefinition => true;
    public bool IsExtensionMethod => false;
    public bool IsExtern => false;
    public bool IsUnsafe => AdapterMethod.IsUnsafe;
    public bool IsGenericMethod => false;
    public bool IsOverride => false;
    public bool IsReadOnly => false;
    public bool IsFinal => true;
    public bool IsVirtual => false;
    public bool IsIterator => false;
    public IteratorMethodKind IteratorKind => IteratorMethodKind.None;
    public TypeSymbol? IteratorElementType => null;
    public ImmutableArray<MethodSymbol> ExplicitInterfaceImplementations => [];
    public ImmutableArray<TypeParameterSymbol> TypeParameters => [];
    public ImmutableArray<TypeSymbol> TypeArguments => [];
    public MethodSymbol? ConstructedFrom => this;
    public bool SetsRequiredMembers => false;

    public ImmutableArray<AttributeData> GetReturnTypeAttributes() => AdapterMethod.GetReturnTypeAttributes();

    public MethodSymbol Construct(params TypeSymbol[] typeArguments)
    {
        if (typeArguments.Length != 0)
            throw new InvalidOperationException("Framework projection methods in the standard catalog are not generic.");

        return this;
    }

    public override void Accept(SymbolVisitor visitor) => visitor.VisitMethod(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitMethod(this);
}
