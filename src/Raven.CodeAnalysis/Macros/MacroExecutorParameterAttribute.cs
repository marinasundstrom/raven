namespace Raven.CodeAnalysis.Macros;

[AttributeUsage(AttributeTargets.Class, AllowMultiple = true, Inherited = false)]
public sealed class MacroExecutorParameterAttribute : Attribute
{
    public MacroExecutorParameterAttribute(
        string name,
        Type runtimeType,
        string typeDisplayName,
        MacroParameterSource source,
        int declarationOrdinal,
        int invocationArgumentOrdinal,
        bool isRequired,
        string defaultValueDisplay)
    {
        Parameter = new MacroExecutorParameter(
            name,
            runtimeType,
            typeDisplayName,
            source,
            declarationOrdinal,
            invocationArgumentOrdinal,
            isRequired,
            defaultValueDisplay);
    }

    internal MacroExecutorParameter Parameter { get; }
}

[AttributeUsage(AttributeTargets.Class, AllowMultiple = true, Inherited = false)]
public sealed class MacroExecutorTypeParameterAttribute : Attribute
{
    public MacroExecutorTypeParameterAttribute(string name, int ordinal)
    {
        Name = name;
        Ordinal = ordinal;
    }

    public string Name { get; }

    public int Ordinal { get; }
}
