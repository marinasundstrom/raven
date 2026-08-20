using System;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Macros;

namespace Raven.CodeAnalysis.Symbols;

internal sealed partial class SourceMacroSymbol : SourceSymbol, IMacroDeclarationSymbol
{
    private MacroTarget _targets;
    private string? _targetName;
    private IParameterSymbol? _targetParameter;
    private bool _isAttached;
    private MacroInvocationTargets _invocationTargets = MacroInvocationTargets.Expression;
    private ImmutableArray<MacroParameterBinding> _parameterBindings = [];

    public SourceMacroSymbol(
        Compilation compilation,
        string name,
        ITypeSymbol returnType,
        ISymbol containingSymbol,
        INamespaceSymbol containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        Accessibility declaredAccessibility)
        : base(
            SymbolKind.Macro,
            name,
            containingSymbol,
            containingType: null,
            containingNamespace,
            locations,
            declaringSyntaxReferences,
            declaredAccessibility)
    {
        DefinitionType = new SynthesizedMacroDefinitionTypeSymbol(
            compilation,
            name,
            containingNamespace,
            locations,
            declaringSyntaxReferences,
            declaredAccessibility);
        ExpandMethod = new SourceMethodSymbol(
            "Expand",
            returnType,
            ImmutableArray<SourceParameterSymbol>.Empty,
            DefinitionType,
            DefinitionType,
            containingNamespace,
            locations,
            declaringSyntaxReferences,
            isStatic: false,
            methodKind: MethodKind.Ordinary,
            declaredAccessibility: Accessibility.Public);
    }

    public override string MetadataName => Name;

    public override bool IsStatic => true;

    public override bool CanBeReferencedByName => true;

    public MacroApplicationKind ApplicationKind =>
        _isAttached
            ? MacroApplicationKind.Attached
            : MacroApplicationKind.Freestanding;

    public MacroInvocationTargets InvocationTargets =>
        _isAttached
            ? MacroInvocationTargets.None
            : _invocationTargets;

    public MacroKind MacroKind =>
        _isAttached
            ? MacroKind.AttachedDeclaration
            : MacroKind.Invocable;

    public MacroTarget Targets => _targets;

    public string? TargetName => _targetName;

    public IParameterSymbol? TargetParameter => _targetParameter;

    public INamedTypeSymbol DefinitionType { get; }

    internal SourceMethodSymbol SourceExpandMethod => (SourceMethodSymbol)ExpandMethod;

    public IMethodSymbol ExpandMethod { get; }

    public ITypeSymbol ReturnType => ExpandMethod.ReturnType;

    public ImmutableArray<IParameterSymbol> Parameters => ExpandMethod.Parameters;

    public ImmutableArray<ITypeParameterSymbol> TypeParameters => DefinitionType.TypeParameters;

    public ImmutableArray<ITypeSymbol> TypeArguments => DefinitionType.TypeArguments;

    public ImmutableArray<MacroParameterBinding> ParameterBindings => _parameterBindings;

    public IMacroDeclarationSymbol OriginalDefinition => this;

    public IMacroDeclarationSymbol Construct(params ITypeSymbol[] typeArguments)
    {
        if (typeArguments.Length != DefinitionType.Arity)
        {
            throw new ArgumentException(
                $"Macro '{Name}' expects {DefinitionType.Arity} type arguments but received {typeArguments.Length}.",
                nameof(typeArguments));
        }

        return typeArguments.Length == 0
            ? this
            : new ConstructedMacroDeclarationSymbol(this, typeArguments.ToImmutableArray());
    }

    internal void SetReturnType(ITypeSymbol returnType)
    {
        SourceExpandMethod.SetReturnType(returnType);
    }

    internal void SetInvocationTargets(MacroInvocationTargets invocationTargets)
    {
        _invocationTargets = invocationTargets;
    }

    internal void SetParameters(ImmutableArray<SourceParameterSymbol> parameters)
    {
        SourceExpandMethod.SetParameters(parameters);

        var bindings = ImmutableArray.CreateBuilder<MacroParameterBinding>(parameters.Length);
        var invocationOrdinal = 0;
        for (var declarationOrdinal = 0; declarationOrdinal < parameters.Length; declarationOrdinal++)
        {
            var parameter = parameters[declarationOrdinal];
            var source = parameter.MacroRole switch
            {
                MacroParameterRole.SyntaxInput => MacroParameterSource.SyntaxInput,
                MacroParameterRole.Context => MacroParameterSource.Context,
                MacroParameterRole.TokenBody => MacroParameterSource.TokenBody,
                MacroParameterRole.AttachedTarget => MacroParameterSource.AttachedTarget,
                _ => MacroParameterSource.Value,
            };
            var acceptsInvocationArgument = source is
                MacroParameterSource.Value or MacroParameterSource.SyntaxInput;
            bindings.Add(new MacroParameterBinding(
                parameter,
                source,
                declarationOrdinal,
                acceptsInvocationArgument ? invocationOrdinal++ : null));
        }

        _parameterBindings = bindings.MoveToImmutable();
    }

    internal void SetTypeParameters(ImmutableArray<ITypeParameterSymbol> typeParameters)
    {
        ((SourceNamedTypeSymbol)DefinitionType).SetTypeParameters(typeParameters);
    }

    internal void SetTarget(
        MacroTarget targets,
        SourceParameterSymbol targetParameter)
    {
        _isAttached = true;
        _targets = targets;
        _targetName = targetParameter.Name;
        _targetParameter = targetParameter;
    }
}
