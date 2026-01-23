namespace Raven.CodeAnalysis;

internal sealed partial class BoundPropagateExpression : BoundExpression
{
    public BoundPropagateExpression(
        BoundExpression operand,
        ITypeSymbol okType,
        ITypeSymbol errorType,
        INamedTypeSymbol enclosingResultType,
        IMethodSymbol enclosingErrorConstructor,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : this(
            operand,
            okType,
            errorType,
            enclosingResultType,
            enclosingErrorConstructor,
            okCaseType: null,
            okValueProperty: null,
            unwrapErrorMethod: null,
            errorConversion: default,
            reason: reason)
    {
    }

    public BoundPropagateExpression(
        BoundExpression operand,
        ITypeSymbol okType,
        ITypeSymbol errorType,
        INamedTypeSymbol enclosingResultType,
        IMethodSymbol enclosingErrorConstructor,
        ITypeSymbol? okCaseType,
        IPropertySymbol? okValueProperty,
        IMethodSymbol? unwrapErrorMethod,
        Conversion errorConversion,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(okType, operand.Symbol, reason)
    {
        Operand = operand;
        OkType = okType;
        ErrorType = errorType;
        EnclosingResultType = enclosingResultType;
        EnclosingErrorConstructor = enclosingErrorConstructor;
        OkCaseType = okCaseType;
        OkValueProperty = okValueProperty;
        UnwrapErrorMethod = unwrapErrorMethod;
        ErrorConversion = errorConversion;
    }

    /// <summary>
    /// The expression being unwrapped (must be a Result&lt;T,E&gt;).
    /// </summary>
    public BoundExpression Operand { get; }

    /// <summary>
    /// The Ok payload type (T) of the operand Result&lt;T,E&gt;.
    /// This is also the type of this bound expression.
    /// </summary>
    public ITypeSymbol OkType { get; }

    /// <summary>
    /// The Error payload type (E) of the operand Result&lt;T,E&gt;.
    /// </summary>
    public ITypeSymbol ErrorType { get; }

    /// <summary>
    /// The enclosing Result return type (Result&lt;U,F&gt;) that this propagate expression targets.
    /// Lowering/codegen will early-return Error(F) constructed via <see cref="EnclosingErrorConstructor"/>.
    /// </summary>
    public INamedTypeSymbol EnclosingResultType { get; }

    /// <summary>
    /// The constructor/method used to build the enclosing Error case for early return.
    /// </summary>
    public IMethodSymbol EnclosingErrorConstructor { get; }

    /// <summary>
    /// The concrete Ok case type (e.g. Result&lt;T,E&gt;.Ok) if known.
    /// Useful for lowering via `TryGetOk(out OkCase)`.
    /// </summary>
    public ITypeSymbol? OkCaseType { get; }

    /// <summary>
    /// The property on the Ok case that yields the payload (typically `Value`) if known.
    /// </summary>
    public IPropertySymbol? OkValueProperty { get; }

    /// <summary>
    /// The extension/instance method used to extract the error payload (e.g. `UnwrapError()`) if known.
    /// </summary>
    public IMethodSymbol? UnwrapErrorMethod { get; }

    /// <summary>
    /// The classified conversion from the operand's error payload type to the enclosing function's error type.
    /// </summary>
    public Conversion ErrorConversion { get; }
}
