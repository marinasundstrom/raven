using System;

namespace Raven.CodeAnalysis.Macros;

public interface IMacroDefinition
{
    /// <summary>
    /// Gets the namespace containing the macro's canonical identity.
    /// </summary>
    string Namespace => GetType().Namespace ?? string.Empty;

    /// <summary>
    /// Gets the declared name forming the final segment of the macro's
    /// canonical identity.
    /// </summary>
    string Name { get; }

    /// <summary>
    /// Gets an optional alternate unqualified invocation name.
    /// </summary>
    string? Alias => null;

    bool AcceptsArguments => false;
}

public interface IMacroDefinition<TParameters> : IMacroDefinition
    where TParameters : class
{
    Type ParametersType => typeof(TParameters);

    bool IMacroDefinition.AcceptsArguments => true;
}
