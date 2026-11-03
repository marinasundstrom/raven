using System;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class UnionFieldUtilities
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

    internal static FieldSymbol? TryGetPayloadField(NamedTypeSymbol unionType, UnionCaseTypeSymbol caseSymbol)
    {
        var target = $"{caseSymbol.Name}Payload";
        foreach (var field in unionType.GetMembers().OfType<FieldSymbol>())
        {
            if (string.Equals(NormalizeFieldName(field.Name), target, StringComparison.Ordinal))
                return field;
        }

        return TryGetAnyPayloadField(unionType);
    }

    internal static FieldSymbol? TryGetAnyPayloadField(NamedTypeSymbol unionType)
        => unionType.GetMembers()
            .OfType<FieldSymbol>()
            .FirstOrDefault(field => IsPayloadFieldName(field.Name));

    internal static FieldSymbol GetRequiredPayloadField(NamedTypeSymbol unionType, UnionCaseTypeSymbol caseSymbol)
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
