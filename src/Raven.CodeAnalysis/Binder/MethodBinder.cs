using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

class MethodBinder : TypeMemberBinder
{
    private readonly object _scopeStateGate = new();
    private readonly IMethodSymbol _methodSymbol;
    private MethodScopeState? _scopeState;
    private int _scopeStateBuildCount;

    public MethodBinder(IMethodSymbol methodSymbol, Binder parent)
        : base(parent, (INamedTypeSymbol)methodSymbol.ContainingType!)
    {
        _methodSymbol = methodSymbol;
    }

    public MethodBinder(IMethodSymbol methodSymbol, Binder parent, IEnumerable<IParameterSymbol> parameters)
        : this(methodSymbol, parent)
    {
        // Parameters are retrieved from _methodSymbol; no additional storage needed
    }

    public override ISymbol ContainingSymbol => _methodSymbol;

    public override ITypeSymbol? LookupType(string name)
    {
        var methodTypeParameter = _methodSymbol.TypeParameters.FirstOrDefault(typeParameter => typeParameter.Name == name);
        if (methodTypeParameter is not null)
            return methodTypeParameter;

        return base.LookupType(name);
    }

    public override ISymbol? LookupSymbol(string name)
    {
        var paramSymbol = GetScopeState().ParametersByName.GetValueOrDefault(name);
        if (paramSymbol is not null)
            return paramSymbol;

        var parentSymbol = base.LookupSymbol(name);
        if (parentSymbol != null)
            return parentSymbol;

        return Compilation.SymbolLookup.GetGlobalMembersSourceFirst(name).FirstOrDefault();
    }

    public override ISymbol? BindDeclaredSymbol(SyntaxNode node)
    {
        if (node is MethodDeclarationSyntax
            or ConstructorDeclarationSyntax
            or ParameterlessConstructorDeclarationSyntax
            or InitializerBlockDeclarationSyntax
            or FinallyDeclarationSyntax
            or OperatorDeclarationSyntax
            or ConversionOperatorDeclarationSyntax
            or AccessorDeclarationSyntax
            or PropertyDeclarationSyntax)
            return _methodSymbol;

        if (node is ParameterSyntax parameter)
        {
            return GetScopeState().ParametersBySyntax.GetValueOrDefault(CreateSyntaxReferenceKey(parameter));
        }

        if (node is ArrowTypeClauseSyntax)
            return _methodSymbol;

        return base.BindDeclaredSymbol(node);
    }

    public IMethodSymbol GetMethodSymbol() => _methodSymbol;

    internal ImmutableArray<IParameterSymbol> GetCachedParametersForTesting()
        => GetScopeState().Parameters;

    internal int ScopeStateBuildCountForTesting => _scopeStateBuildCount;

    protected override IReadOnlyDictionary<string, ITypeSymbol> GetInScopeTypeParameters()
    {
        var map = new Dictionary<string, ITypeSymbol>(StringComparer.Ordinal);

        if (!_methodSymbol.TypeParameters.IsDefaultOrEmpty)
        {
            foreach (var tp in _methodSymbol.TypeParameters)
                map.TryAdd(tp.Name, tp);
        }

        var containingType = _methodSymbol.ContainingType;
        if (containingType is null || containingType.TypeParameters.IsDefaultOrEmpty)
            return map;

        foreach (var tp in containingType.TypeParameters)
            map.TryAdd(tp.Name, tp);

        return map;
    }

    private MethodScopeState GetScopeState()
    {
        var signatureVersion = GetSignatureVersion();
        var state = _scopeState;
        if (state is not null && state.SignatureVersion == signatureVersion)
            return state;

        lock (_scopeStateGate)
        {
            state = _scopeState;
            if (state is not null && state.SignatureVersion == signatureVersion)
                return state;

            state = CreateScopeState(signatureVersion);
            _scopeState = state;
            _scopeStateBuildCount++;
            return state;
        }
    }

    private int GetSignatureVersion()
        => _methodSymbol is SourceMethodSymbol sourceMethod
            ? sourceMethod.SignatureVersion
            : 0;

    private MethodScopeState CreateScopeState(int signatureVersion)
    {
        var parameters = _methodSymbol.Parameters;
        var parametersByName = ImmutableDictionary.CreateBuilder<string, IParameterSymbol>(StringComparer.Ordinal);
        var parametersBySyntax = ImmutableDictionary.CreateBuilder<SyntaxReferenceKey, IParameterSymbol>();

        foreach (var parameter in parameters)
        {
            parametersByName.TryAdd(parameter.Name, parameter);

            foreach (var reference in parameter.DeclaringSyntaxReferences)
                parametersBySyntax.TryAdd(new SyntaxReferenceKey(reference.SyntaxTree, reference.Span), parameter);
        }

        var typeParameters = _methodSymbol.TypeParameters;
        var typeParametersByName = ImmutableDictionary.CreateBuilder<string, ITypeSymbol>(StringComparer.Ordinal);
        foreach (var typeParameter in typeParameters)
            typeParametersByName.TryAdd(typeParameter.Name, typeParameter);

        return new MethodScopeState(
            signatureVersion,
            parameters,
            parametersByName.ToImmutable(),
            parametersBySyntax.ToImmutable(),
            typeParameters,
            typeParametersByName.ToImmutable());
    }

    private static SyntaxReferenceKey CreateSyntaxReferenceKey(SyntaxNode node)
        => new(node.SyntaxTree, node.Span);

    private sealed record MethodScopeState(
        int SignatureVersion,
        ImmutableArray<IParameterSymbol> Parameters,
        ImmutableDictionary<string, IParameterSymbol> ParametersByName,
        ImmutableDictionary<SyntaxReferenceKey, IParameterSymbol> ParametersBySyntax,
        ImmutableArray<ITypeParameterSymbol> TypeParameters,
        ImmutableDictionary<string, ITypeSymbol> TypeParametersByName);

    private readonly record struct SyntaxReferenceKey(SyntaxTree SyntaxTree, TextSpan Span);

    protected override bool IsInUnsafeContext
    {
        get
        {
            if (_methodSymbol.IsUnsafe)
                return true;

            foreach (var reference in _methodSymbol.DeclaringSyntaxReferences)
            {
                var syntax = reference.GetSyntax();
                var hasUnsafeModifier = syntax switch
                {
                    MethodDeclarationSyntax methodDeclaration => methodDeclaration.Modifiers.Any(m => m.Kind == SyntaxKind.UnsafeKeyword),
                    ConstructorDeclarationSyntax constructorDeclaration => constructorDeclaration.Modifiers.Any(m => m.Kind == SyntaxKind.UnsafeKeyword),
                    ParameterlessConstructorDeclarationSyntax initDeclaration => initDeclaration.Modifiers.Any(m => m.Kind == SyntaxKind.UnsafeKeyword),
                    InitializerBlockDeclarationSyntax initBlockDeclaration => initBlockDeclaration.Modifiers.Any(m => m.Kind == SyntaxKind.UnsafeKeyword),
                    FinallyDeclarationSyntax finalDeclaration => finalDeclaration.Modifiers.Any(m => m.Kind == SyntaxKind.UnsafeKeyword),
                    FunctionStatementSyntax functionStatement => functionStatement.Modifiers.Any(m => m.Kind == SyntaxKind.UnsafeKeyword),
                    _ => false,
                };

                if (hasUnsafeModifier)
                    return true;
            }

            return base.IsInUnsafeContext;
        }
    }
}
