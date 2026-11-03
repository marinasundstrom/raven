using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Macros;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class ConstructedMacroDeclarationSymbol : SymbolBase, MacroDeclarationSymbol
{
    private readonly MacroDeclarationSymbol _originalDefinition;
    private readonly ImmutableArray<MacroParameterBinding> _parameterBindings;

    public ConstructedMacroDeclarationSymbol(
        MacroDeclarationSymbol originalDefinition,
        ImmutableArray<TypeSymbol> typeArguments)
        : base(
            SymbolKind.Macro,
            originalDefinition.Name,
            originalDefinition.ContainingSymbol,
            containingType: null,
            originalDefinition.ContainingNamespace,
            originalDefinition.Locations.ToArray(),
            originalDefinition.DeclaringSyntaxReferences.ToArray(),
            originalDefinition.DeclaredAccessibility)
    {
        _originalDefinition = originalDefinition.OriginalDefinition;
        DefinitionType = (NamedTypeSymbol)_originalDefinition.DefinitionType.Construct(typeArguments.ToArray());
        ExpandMethod = DefinitionType.GetMembers("Expand").OfType<MethodSymbol>().Single();

        var bindings = ImmutableArray.CreateBuilder<MacroParameterBinding>(
            _originalDefinition.ParameterBindings.Length);
        foreach (var binding in _originalDefinition.ParameterBindings)
        {
            bindings.Add(new MacroParameterBinding(
                ExpandMethod.Parameters[binding.DeclarationOrdinal],
                binding.Source,
                binding.DeclarationOrdinal,
                binding.InvocationArgumentOrdinal));
        }

        _parameterBindings = bindings.MoveToImmutable();
    }

    public override string MetadataName => Name;

    public override bool IsStatic => true;

    public override bool CanBeReferencedByName => true;

    public MacroApplicationKind ApplicationKind => _originalDefinition.ApplicationKind;

    public MacroInvocationTargets InvocationTargets => _originalDefinition.InvocationTargets;

    public TypeSymbol? ExpressionResultType =>
        ReturnType is NamedTypeSymbol { TypeArguments.Length: 1 } namedType &&
        MacroParameterRoleFacts.IsExpressionSyntaxFacade(namedType)
            ? namedType.TypeArguments[0]
            : null;

    public MacroKind MacroKind => _originalDefinition.MacroKind;

    public MacroTarget Targets => _originalDefinition.Targets;

    public string? TargetName => _originalDefinition.TargetName;

    public ParameterSymbol? TargetParameter => _originalDefinition.TargetParameter is null
        ? null
        : Parameters[_originalDefinition.Parameters.IndexOf(_originalDefinition.TargetParameter)];

    public NamedTypeSymbol DefinitionType { get; }

    public MethodSymbol ExpandMethod { get; }

    public TypeSymbol ReturnType => ExpandMethod.ReturnType;

    public ImmutableArray<ParameterSymbol> Parameters => ExpandMethod.Parameters;

    public ImmutableArray<MacroParameterBinding> ParameterBindings => _parameterBindings;

    public ImmutableArray<TypeParameterSymbol> TypeParameters => DefinitionType.TypeParameters;

    public ImmutableArray<TypeSymbol> TypeArguments => DefinitionType.TypeArguments;

    public int Arity => DefinitionType.Arity;

    public MacroDeclarationSymbol OriginalDefinition => _originalDefinition;

    public MacroDeclarationSymbol Construct(params TypeSymbol[] typeArguments)
        => _originalDefinition.Construct(typeArguments);

    public override DocumentationComment? GetDocumentationComment()
        => _originalDefinition.GetDocumentationComment();

    public override void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);

    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);
}
