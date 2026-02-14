using System;
using System.Collections.Immutable;
using System.Threading;

namespace Raven.CodeAnalysis.Symbols;

internal sealed partial class SourceLambdaSymbol : SourceSymbol, ILambdaSymbol
{
    private static int _fallbackLambdaOrdinal;
    private ImmutableArray<ISymbol> _capturedVariables = ImmutableArray<ISymbol>.Empty;
    private bool _hasAsyncReturnTypeError;
    private bool _containsAwait;
    private bool _isExpressionTreeLambda;
    private SynthesizedAsyncStateMachineTypeSymbol? _asyncStateMachine;

    public SourceLambdaSymbol(
        IReadOnlyList<IParameterSymbol> parameters,
        ITypeSymbol returnType,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType = null,
        INamespaceSymbol? containingNamespace = null,
        Location[]? locations = null,
        SyntaxReference[]? declaringSyntaxReferences = null,
        bool isAsync = false)
        : base(
            SymbolKind.Method,
            CreateLambdaName(locations),
            containingSymbol,
            containingType,
            containingNamespace,
            locations ?? Array.Empty<Location>(),
            declaringSyntaxReferences ?? Array.Empty<SyntaxReference>())
    {
        Parameters = parameters.ToImmutableArray();
        ReturnType = returnType;
        IsAsync = isAsync;
    }

    public ImmutableArray<IParameterSymbol> Parameters { get; }

    public ITypeSymbol ReturnType { get; private set; }

    public bool IsConstructor => false;

    public override bool IsStatic => !HasCaptures;

    public MethodKind MethodKind => MethodKind.LambdaMethod;

    internal bool HasAsyncReturnTypeError => _hasAsyncReturnTypeError;

    public IMethodSymbol? OriginalDefinition => null;

    public ImmutableArray<ITypeParameterSymbol> TypeParameters => ImmutableArray<ITypeParameterSymbol>.Empty;

    public ImmutableArray<ITypeSymbol> TypeArguments => ImmutableArray<ITypeSymbol>.Empty;

    public IMethodSymbol? ConstructedFrom => null;

    public bool IsAbstract => false;
    public bool IsAsync { get; }
    public bool IsCheckedBuiltin => false;
    public bool IsDefinition => true;
    public bool IsExtensionMethod => false;
    public bool IsExtern => false;
    public bool IsGenericMethod => false;
    public bool IsOverride => false;
    public bool IsReadOnly => false;
    public bool IsFinal => false;
    public bool IsVirtual => false;
    public bool IsIterator => false;
    public IteratorMethodKind IteratorKind => IteratorMethodKind.None;
    public ITypeSymbol? IteratorElementType => null;

    public ImmutableArray<IMethodSymbol> ExplicitInterfaceImplementations => ImmutableArray<IMethodSymbol>.Empty;

    public ImmutableArray<AttributeData> GetReturnTypeAttributes() => ImmutableArray<AttributeData>.Empty;

    public void SetReturnType(ITypeSymbol returnType)
    {
        ReturnType = returnType;
    }

    public bool ContainsAwait => _containsAwait;
    internal bool IsExpressionTreeLambda => _isExpressionTreeLambda;

    internal SynthesizedAsyncStateMachineTypeSymbol? AsyncStateMachine => _asyncStateMachine;

    internal void MarkAsyncReturnTypeError()
    {
        _hasAsyncReturnTypeError = true;
    }

    public ITypeSymbol? DelegateType { get; private set; }

    public ImmutableArray<ISymbol> CapturedVariables => _capturedVariables;

    public bool HasCaptures => !_capturedVariables.IsDefaultOrEmpty && _capturedVariables.Length > 0;

    public bool SetsRequiredMembers => false;

    public void SetDelegateType(ITypeSymbol delegateType)
    {
        DelegateType = delegateType;
    }

    public void SetCapturedVariables(IEnumerable<ISymbol> capturedVariables)
    {
        _capturedVariables = capturedVariables.ToImmutableArray();
    }

    public void SetContainsAwait(bool containsAwait)
    {
        _containsAwait = containsAwait;
    }

    internal void MarkExpressionTreeLambda()
    {
        _isExpressionTreeLambda = true;
    }

    internal void SetAsyncStateMachine(SynthesizedAsyncStateMachineTypeSymbol stateMachine)
    {
        _asyncStateMachine = stateMachine ?? throw new ArgumentNullException(nameof(stateMachine));
    }

    public IMethodSymbol Construct(params ITypeSymbol[] typeArguments)
    {
        throw new NotSupportedException("Lambdas cannot be constructed with type arguments.");
    }

    private static string CreateLambdaName(Location[]? locations)
    {
        if (locations is { Length: > 0 } &&
            locations[0] is { } first &&
            first.SourceSpan.Start >= 0)
        {
            return $"<lambda_{first.SourceSpan.Start}>";
        }

        var ordinal = Interlocked.Increment(ref _fallbackLambdaOrdinal);
        return $"<lambda_{ordinal}>";
    }
}
