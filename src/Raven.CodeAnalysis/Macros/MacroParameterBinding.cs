namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Identifies who supplies a parameter of a macro's designated expansion
/// method.
/// </summary>
public enum MacroParameterSource
{
    Value,
    SyntaxInput,
    Context,
    TokenBody,
    AttachedTarget,
}

/// <summary>
/// Associates one canonical expansion-method parameter with its declaration
/// and invocation positions.
/// </summary>
public sealed class MacroParameterBinding
{
    internal MacroParameterBinding(
        IParameterSymbol parameter,
        MacroParameterSource source,
        int declarationOrdinal,
        int? invocationArgumentOrdinal)
    {
        Parameter = parameter;
        Source = source;
        DeclarationOrdinal = declarationOrdinal;
        InvocationArgumentOrdinal = invocationArgumentOrdinal;
    }

    public IParameterSymbol Parameter { get; }

    public MacroParameterSource Source { get; }

    public int DeclarationOrdinal { get; }

    public int? InvocationArgumentOrdinal { get; }
}
