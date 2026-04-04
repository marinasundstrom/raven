using System;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Symbols;

internal sealed partial class SourceLambdaSymbol : SourceSymbol, ILambdaSymbol
{
    private ImmutableArray<IParameterSymbol> _parameters;
    private ImmutableArray<ITypeParameterSymbol> _typeParameters = ImmutableArray<ITypeParameterSymbol>.Empty;
    private ImmutableArray<ITypeSymbol> _typeArguments = ImmutableArray<ITypeSymbol>.Empty;
    private ImmutableArray<ISymbol> _capturedVariables = ImmutableArray<ISymbol>.Empty;
    private bool _hasAsyncReturnTypeError;
    private bool _containsAwait;
    private bool _isExpressionTreeLambda;
    private bool _shouldDeferAsyncReturnDiagnostics;
    private bool _isIterator;
    private IteratorMethodKind _iteratorKind;
    private ITypeSymbol? _iteratorElementType;
    private SynthesizedAsyncStateMachineTypeSymbol? _asyncStateMachine;
    private SynthesizedIteratorTypeSymbol? _iteratorStateMachine;
    private SourceNamedTypeSymbol? _closureFrameType;

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
            CreateLambdaName(containingSymbol, locations),
            containingSymbol,
            containingType,
            containingNamespace,
            locations ?? Array.Empty<Location>(),
            declaringSyntaxReferences ?? Array.Empty<SyntaxReference>())
    {
        _parameters = parameters.ToImmutableArray();
        ReturnType = returnType;
        IsAsync = isAsync;
    }

    public bool IsLambda => true;

    public ImmutableArray<IParameterSymbol> Parameters => _parameters;

    public ITypeSymbol ReturnType { get; private set; }

    public bool IsConstructor => false;

    // Align with C# closure lowering: lambdas are emitted as instance methods on
    // compiler-generated closure carrier types (including non-capturing lambdas).
    public override bool IsStatic => false;

    public MethodKind MethodKind => MethodKind.LambdaMethod;

    internal bool HasAsyncReturnTypeError => _hasAsyncReturnTypeError;
    internal bool ShouldDeferAsyncReturnDiagnostics => _shouldDeferAsyncReturnDiagnostics;

    public IMethodSymbol? OriginalDefinition => this;

    public ImmutableArray<ITypeParameterSymbol> TypeParameters => _typeParameters;

    public ImmutableArray<ITypeSymbol> TypeArguments => _typeArguments;

    public IMethodSymbol? ConstructedFrom => this;

    public bool IsAbstract => false;
    public bool IsAsync { get; }
    public bool IsCheckedBuiltin => false;
    public bool IsDefinition => true;
    public bool IsExtensionMethod => false;
    public bool IsExtern => false;
    public bool IsUnsafe => false;
    public bool IsGenericMethod => !_typeParameters.IsDefaultOrEmpty && _typeParameters.Length > 0;
    public bool IsOverride => false;
    public bool IsReadOnly => false;
    public bool IsFinal => false;
    public bool IsVirtual => false;
    public bool IsIterator => _isIterator;
    public IteratorMethodKind IteratorKind => _iteratorKind;
    public ITypeSymbol? IteratorElementType => _iteratorElementType;
    public SynthesizedIteratorTypeSymbol? IteratorStateMachine => _iteratorStateMachine;

    public ImmutableArray<IMethodSymbol> ExplicitInterfaceImplementations => ImmutableArray<IMethodSymbol>.Empty;

    public ImmutableArray<AttributeData> GetReturnTypeAttributes() => ImmutableArray<AttributeData>.Empty;

    public void SetReturnType(ITypeSymbol returnType)
    {
        ReturnType = returnType;
    }

    internal void SetParameters(IEnumerable<IParameterSymbol> parameters)
    {
        _parameters = parameters.ToImmutableArray();
    }

    internal void SetTypeParameters(IEnumerable<ITypeParameterSymbol> typeParameters)
    {
        _typeParameters = typeParameters.ToImmutableArray();
        _typeArguments = _typeParameters.IsDefaultOrEmpty
            ? ImmutableArray<ITypeSymbol>.Empty
            : _typeParameters.Select(static tp => (ITypeSymbol)tp).ToImmutableArray();
    }

    public bool ContainsAwait => _containsAwait;
    internal bool IsExpressionTreeLambda => _isExpressionTreeLambda;

    internal SynthesizedAsyncStateMachineTypeSymbol? AsyncStateMachine => _asyncStateMachine;

    internal void MarkAsyncReturnTypeError()
    {
        _hasAsyncReturnTypeError = true;
    }

    internal void SetShouldDeferAsyncReturnDiagnostics(bool shouldDefer)
    {
        _shouldDeferAsyncReturnDiagnostics = shouldDefer;
    }

    public ITypeSymbol? DelegateType { get; private set; }

    public ImmutableArray<ISymbol> CapturedVariables => _capturedVariables;

    public bool HasCaptures => !_capturedVariables.IsDefaultOrEmpty && _capturedVariables.Length > 0;

    public bool SetsRequiredMembers => false;
    internal SourceNamedTypeSymbol? ClosureFrameType => _closureFrameType;

    public void SetDelegateType(ITypeSymbol delegateType)
    {
        DelegateType = delegateType;
    }

    public void SetCapturedVariables(IEnumerable<ISymbol> capturedVariables)
    {
        _capturedVariables = capturedVariables.ToImmutableArray();
    }

    internal void SetClosureFrameType(SourceNamedTypeSymbol? closureFrameType)
    {
        _closureFrameType = closureFrameType;
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

    internal void MarkIterator(IteratorMethodKind kind, ITypeSymbol elementType)
    {
        if (kind == IteratorMethodKind.None)
            return;

        _isIterator = true;
        _iteratorKind = kind;
        _iteratorElementType = elementType;
    }

    internal void SetIteratorStateMachine(SynthesizedIteratorTypeSymbol stateMachine)
    {
        if (stateMachine is null)
            throw new ArgumentNullException(nameof(stateMachine));

        if (_iteratorStateMachine is not null && !ReferenceEquals(_iteratorStateMachine, stateMachine))
            throw new InvalidOperationException("Iterator state machine already assigned.");

        _iteratorStateMachine = stateMachine;
    }

    public IMethodSymbol Construct(params ITypeSymbol[] typeArguments)
    {
        if (typeArguments is null)
            throw new ArgumentNullException(nameof(typeArguments));

        return new ConstructedMethodSymbol(this, typeArguments.ToImmutableArray());
    }

    private static string CreateLambdaName(ISymbol containingSymbol, Location[]? locations)
    {
        var containerName = GetLambdaContainerName(containingSymbol);
        var ordinal = ComputeLambdaOrdinal(containingSymbol, locations);
        var pathPrefix = GetEnclosingLambdaOrdinalPath(containingSymbol);
        var ordinalPath = pathPrefix is null ? ordinal.ToString() : $"{pathPrefix}_{ordinal}";
        return $"<{containerName}>b__{ordinalPath}";
    }

    private static string GetLambdaContainerName(ISymbol containingSymbol)
    {
        if (containingSymbol is not IMethodSymbol method)
            return "lambda";

        var name = method.Name;
        if (string.IsNullOrWhiteSpace(name))
            return "lambda";

        if (method.MethodKind == MethodKind.LambdaMethod &&
            name.Length > 2 &&
            name[0] == '<')
        {
            var closing = name.IndexOf('>');
            if (closing > 1)
                return name.Substring(1, closing - 1);
        }

        return name;
    }

    private static int ComputeLambdaOrdinal(ISymbol containingSymbol, Location[]? locations)
    {
        if (locations is not { Length: > 0 } ||
            locations[0] is not { SourceSpan.Start: >= 0 } first)
        {
            return 0;
        }

        var targetStart = first.SourceSpan.Start;
        if (containingSymbol is not IMethodSymbol method)
            return 0;

        foreach (var syntaxRef in method.DeclaringSyntaxReferences)
        {
            if (syntaxRef.GetSyntax() is not SyntaxNode root)
                continue;

            var starts = root
                .DescendantNodesAndSelf()
                .OfType<FunctionExpressionSyntax>()
                .Select(static lambda => lambda.Span.Start)
                .Distinct()
                .OrderBy(static start => start)
                .ToArray();

            var index = Array.IndexOf(starts, targetStart);
            if (index >= 0)
                return index;
        }

        return 0;
    }

    private static string? GetEnclosingLambdaOrdinalPath(ISymbol containingSymbol)
    {
        if (containingSymbol is not IMethodSymbol { MethodKind: MethodKind.LambdaMethod } method)
            return null;

        var name = method.Name;
        var marker = ">b__";
        var markerIndex = name.IndexOf(marker, StringComparison.Ordinal);
        if (markerIndex < 0)
            return null;

        var start = markerIndex + marker.Length;
        if (start >= name.Length)
            return null;

        var end = name.IndexOf('>', start);
        if (end < 0)
            end = name.Length;

        var path = name.Substring(start, end - start);
        return string.IsNullOrWhiteSpace(path) ? null : path;
    }
}
