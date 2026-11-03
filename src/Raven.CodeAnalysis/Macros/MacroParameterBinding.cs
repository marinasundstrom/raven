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
    DeclarationInput,
}

/// <summary>
/// Associates one canonical expansion-method parameter with its declaration
/// and invocation positions.
/// </summary>
public sealed record MacroParameterBinding
{
    internal MacroParameterBinding(
        ParameterSymbol parameter,
        MacroParameterSource source,
        int declarationOrdinal,
        int? invocationArgumentOrdinal)
    {
        Parameter = parameter;
        Source = source;
        DeclarationOrdinal = declarationOrdinal;
        InvocationArgumentOrdinal = invocationArgumentOrdinal;
    }

    public ParameterSymbol Parameter { get; init; }

    public MacroParameterSource Source { get; init; }

    public int DeclarationOrdinal { get; init; }

    public int? InvocationArgumentOrdinal { get; init; }

    public void Deconstruct(out ParameterSymbol parameter, out MacroParameterSource source,
        out int declarationOrdinal, out int? invocationArgumentOrdinal)
    {
        parameter = Parameter;
        source = Source;
        declarationOrdinal = DeclarationOrdinal;
        invocationArgumentOrdinal = InvocationArgumentOrdinal;
    }

    public override string ToString() => "Raven.CodeAnalysis.Macros.MacroParameterBinding";
}
