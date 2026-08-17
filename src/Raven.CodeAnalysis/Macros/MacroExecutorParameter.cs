namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Portable declaration metadata for one erased executor parameter.
/// </summary>
public sealed class MacroExecutorParameter
{
    public MacroExecutorParameter(
        string name,
        Type runtimeType,
        string typeDisplayName,
        MacroParameterSource source,
        int declarationOrdinal,
        int invocationArgumentOrdinal,
        bool isRequired,
        string defaultValueDisplay)
    {
        Name = name;
        RuntimeType = runtimeType;
        TypeDisplayName = typeDisplayName;
        Source = source;
        DeclarationOrdinal = declarationOrdinal;
        InvocationArgumentOrdinal = invocationArgumentOrdinal < 0
            ? null
            : invocationArgumentOrdinal;
        IsRequired = isRequired;
        DefaultValueDisplay = defaultValueDisplay;
    }

    public string Name { get; }

    public Type RuntimeType { get; }

    public string TypeDisplayName { get; }

    public MacroParameterSource Source { get; }

    public int DeclarationOrdinal { get; }

    public int? InvocationArgumentOrdinal { get; }

    public bool IsRequired { get; }

    public string DefaultValueDisplay { get; }
}
