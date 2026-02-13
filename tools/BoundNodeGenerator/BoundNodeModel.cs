using System;
using System.Collections.Generic;

using Microsoft.CodeAnalysis.CSharp;

namespace BoundNodeGenerator;

enum ParameterKind
{
    Other,
    BoundNode,
    BoundNodeList,
    Symbol,
    SymbolList,
}

sealed record ParameterModel(
    string PropertyName,
    string ParameterName,
    string TypeName,
    string NonNullableTypeName,
    bool IsNullable,
    ParameterKind Kind,
    string? ElementTypeName,
    bool ElementIsNullable,
    string? DefaultValue)
{
    public bool RequiresRewrite => Kind is ParameterKind.BoundNode or ParameterKind.BoundNodeList or ParameterKind.Symbol or ParameterKind.SymbolList;
}

sealed record BoundNodeModel(
    string Name,
    string Namespace,
    string Accessibility,
    string? BaseTypeName,
    bool IsPartial,
    bool IsAbstract,
    IReadOnlyList<ParameterModel> Parameters)
{
    public bool HasUpdate => IsPartial && Parameters.Count > 0;
    public string VisitorMethodName => Name.StartsWith("Bound", StringComparison.Ordinal)
        ? Name["Bound".Length..]
        : Name;
}

sealed record SymbolClassModel(
    string Name,
    string Namespace,
    string Accessibility)
{
    public string VisitorMethodName
    {
        get
        {
            var name = Name;
            if (name.EndsWith("Symbol", StringComparison.Ordinal))
            {
                name = name[..^"Symbol".Length];
            }

            if (name.StartsWith("Source", StringComparison.Ordinal))
            {
                name = name["Source".Length..];
            }

            if (name.StartsWith("PE", StringComparison.Ordinal))
            {
                name = name["PE".Length..];
            }

            if (name.StartsWith("Merged", StringComparison.Ordinal))
            {
                name = name["Merged".Length..];
            }

            if (name.Length == 0)
            {
                name = Name;
            }

            return name;
        }
    }
}

sealed record SymbolInterfaceModel(string Name, string Namespace);

static class NameHelpers
{
    public static string ToPascalCase(string name)
    {
        if (string.IsNullOrEmpty(name))
            return name;

        if (name.Length == 1)
            return char.ToUpperInvariant(name[0]).ToString();

        return char.ToUpperInvariant(name[0]) + name[1..];
    }

    public static string ToCamelCase(string name)
    {
        if (string.IsNullOrEmpty(name))
            return name;

        if (name.Length == 1)
            return char.ToLowerInvariant(name[0]).ToString();

        return char.ToLowerInvariant(name[0]) + name[1..];
    }

    public static string EscapeIdentifier(string identifier)
    {
        var keyword = SyntaxFacts.GetKeywordKind(identifier);
        return SyntaxFacts.IsKeywordKind(keyword) ? $"@{identifier}" : identifier;
    }
}
