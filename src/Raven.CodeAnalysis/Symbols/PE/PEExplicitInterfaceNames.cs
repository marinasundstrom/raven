namespace Raven.CodeAnalysis.Symbols;

internal static class PEExplicitInterfaceNames
{
    // Explicit implementation names encode the implemented interface, including
    // its actual type arguments, rather than its generic definition's parameters.
    public static string Format(Type type)
    {
        if (type.IsGenericParameter)
            return type.Name;
        if (type.IsArray)
            return Format(type.GetElementType()!) + "[" + new string(',', type.GetArrayRank() - 1) + "]";
        if (!type.IsGenericType)
            return (type.FullName ?? type.Name).Replace('+', '.');

        var definition = type.GetGenericTypeDefinition();
        var name = definition.FullName ?? definition.Name;
        var arguments = type.GetGenericArguments();
        var argumentIndex = 0;
        var parts = name.Split('+');
        for (var i = 0; i < parts.Length; i++)
        {
            var arityStart = parts[i].IndexOf('`');
            if (arityStart < 0)
                continue;

            var arity = int.Parse(parts[i].AsSpan(arityStart + 1), System.Globalization.CultureInfo.InvariantCulture);
            var formattedArguments = arguments.Skip(argumentIndex).Take(arity).Select(Format);
            parts[i] = parts[i][..arityStart] + "<" + string.Join(",", formattedArguments) + ">";
            argumentIndex += arity;
        }
        return string.Join(".", parts);
    }
}
