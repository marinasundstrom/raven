using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Macros;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class ConstructedMacroDeclarationSymbol : Symbol, IMacroDeclarationSymbol
{
    private readonly IMacroDeclarationSymbol _originalDefinition;
    private readonly ImmutableArray<MacroParameterBinding> _parameterBindings;

    public ConstructedMacroDeclarationSymbol(
        IMacroDeclarationSymbol originalDefinition,
        ImmutableArray<ITypeSymbol> typeArguments)
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
        DefinitionType = (INamedTypeSymbol)_originalDefinition.DefinitionType.Construct(typeArguments.ToArray());
        ExpandMethod = DefinitionType.GetMembers("Expand").OfType<IMethodSymbol>().Single();

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

    public ITypeSymbol? ExpressionResultType =>
        ReturnType is INamedTypeSymbol { TypeArguments.Length: 1 } namedType &&
        MacroParameterRoleFacts.IsExpressionSyntaxFacade(namedType)
            ? namedType.TypeArguments[0]
            : null;

    public MacroKind MacroKind => _originalDefinition.MacroKind;

    public MacroTarget Targets => _originalDefinition.Targets;

    public string? TargetName => _originalDefinition.TargetName;

    public IParameterSymbol? TargetParameter => _originalDefinition.TargetParameter is null
        ? null
        : Parameters[_originalDefinition.Parameters.IndexOf(_originalDefinition.TargetParameter)];

    public INamedTypeSymbol DefinitionType { get; }

    public IMethodSymbol ExpandMethod { get; }

    public ITypeSymbol ReturnType => ExpandMethod.ReturnType;

    public ImmutableArray<IParameterSymbol> Parameters => ExpandMethod.Parameters;

    public ImmutableArray<MacroParameterBinding> ParameterBindings => _parameterBindings;

    public ImmutableArray<ITypeParameterSymbol> TypeParameters => DefinitionType.TypeParameters;

    public ImmutableArray<ITypeSymbol> TypeArguments => DefinitionType.TypeArguments;

    public int Arity => DefinitionType.Arity;

    public IMacroDeclarationSymbol OriginalDefinition => _originalDefinition;

    public IMacroDeclarationSymbol Construct(params ITypeSymbol[] typeArguments)
        => _originalDefinition.Construct(typeArguments);

    public override DocumentationComment? GetDocumentationComment()
        => _originalDefinition.GetDocumentationComment();

    public override void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);

    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);
}
