using System;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class DiscriminatedUnionFieldUtilities
{
    internal const string TagFieldName = "<Tag>";
    internal const int TagFieldOffset = 0;
    internal const int PayloadFieldOffset = 8;

    internal static string GetPayloadFieldName(string caseName)
        => $"<{caseName}Payload>";

    internal static bool IsTagFieldName(string name)
        => string.Equals(NormalizeFieldName(name), "Tag", StringComparison.Ordinal);

    internal static bool IsPayloadFieldName(string name)
    {
        var normalized = NormalizeFieldName(name);
        return normalized.EndsWith("Payload", StringComparison.Ordinal);
    }

    internal static IFieldSymbol? TryGetPayloadField(INamedTypeSymbol unionType, IDiscriminatedUnionCaseSymbol caseSymbol)
    {
        var target = $"{caseSymbol.Name}Payload";
        foreach (var field in unionType.GetMembers().OfType<IFieldSymbol>())
        {
            if (string.Equals(NormalizeFieldName(field.Name), target, StringComparison.Ordinal))
                return field;
        }

        return TryGetAnyPayloadField(unionType);
    }

    internal static IFieldSymbol? TryGetAnyPayloadField(INamedTypeSymbol unionType)
        => unionType.GetMembers()
            .OfType<IFieldSymbol>()
            .FirstOrDefault(field => IsPayloadFieldName(field.Name));

    internal static IFieldSymbol GetRequiredPayloadField(INamedTypeSymbol unionType, IDiscriminatedUnionCaseSymbol caseSymbol)
    {
        return TryGetPayloadField(unionType, caseSymbol)
            ?? throw new InvalidOperationException($"Union '{unionType.Name}' is missing backing field for case '{caseSymbol.Name}'.");
    }

    internal static string NormalizeFieldName(string name)
    {
        Span<char> buffer = stackalloc char[name.Length];
        var index = 0;

        foreach (var ch in name)
        {
            if (ch is '<' or '>' or '_')
                continue;

            buffer[index++] = ch;
        }

        return new string(buffer[..index]);
    }
}
