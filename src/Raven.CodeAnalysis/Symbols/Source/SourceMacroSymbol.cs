using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Macros;

namespace Raven.CodeAnalysis.Symbols;

internal sealed partial class SourceMacroSymbol : SourceSymbol, IMacroDeclarationSymbol
{
    private ITypeSymbol _returnType;
    private ImmutableArray<SourceParameterSymbol> _parameters = ImmutableArray<SourceParameterSymbol>.Empty;
    private ImmutableArray<ITypeParameterSymbol> _typeParameters = ImmutableArray<ITypeParameterSymbol>.Empty;
    private MacroTarget _targets;
    private string? _targetName;
    private IParameterSymbol? _targetParameter;
    private bool _isAttached;
    private MacroInvocationTargets _invocationTargets = MacroInvocationTargets.Expression;

    public SourceMacroSymbol(
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
        _returnType = returnType;
    }

    public override string MetadataName => Name;

    public override bool IsStatic => true;

    public override bool CanBeReferencedByName => true;

    public MacroApplicationKind ApplicationKind =>
        _isAttached
            ? MacroApplicationKind.Attached
            : MacroApplicationKind.Invocable;

    public MacroInvocationTargets InvocationTargets =>
        _isAttached
            ? MacroInvocationTargets.None
            : _invocationTargets;

    public MacroKind MacroKind =>
        _isAttached
            ? MacroKind.AttachedDeclaration
            : MacroKind.FreestandingExpression;

    public MacroTarget Targets => _targets;

    public string? TargetName => _targetName;

    public IParameterSymbol? TargetParameter => _targetParameter;

    public ITypeSymbol ReturnType => _returnType;

    public ImmutableArray<IParameterSymbol> Parameters =>
        _parameters.Cast<IParameterSymbol>().ToImmutableArray();

    public ImmutableArray<ITypeParameterSymbol> TypeParameters => _typeParameters;

    internal void SetReturnType(ITypeSymbol returnType)
    {
        _returnType = returnType;
    }

    internal void SetInvocationTargets(MacroInvocationTargets invocationTargets)
    {
        _invocationTargets = invocationTargets;
    }

    internal void SetParameters(ImmutableArray<SourceParameterSymbol> parameters)
    {
        _parameters = parameters;
    }

    internal void SetTypeParameters(ImmutableArray<ITypeParameterSymbol> typeParameters)
    {
        _typeParameters = typeParameters;
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
