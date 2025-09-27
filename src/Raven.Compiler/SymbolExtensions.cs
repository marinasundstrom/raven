using System;
using System.Text;

using Raven.CodeAnalysis;

public static class SymbolExtensions
{
    public static string ToSymbolHierarchyString(this ISymbol symbol, int maxDepth = int.MaxValue)
    {
        return symbol.ToSymbolHierarchyString(static _ => true, maxDepth);
    }

    public static string ToSymbolHierarchyString(this ISymbol symbol, Func<ISymbol, bool> filter, int maxDepth = int.MaxValue)
    {
        var builder = new StringBuilder();
        AppendSymbol(symbol, builder, indent: 0, maxDepth, filter);
        return builder.ToString();
    }

    private static void AppendSymbol(ISymbol symbol, StringBuilder builder, int indent, int maxDepth, Func<ISymbol, bool> filter)
    {
        if (indent > maxDepth)
            return;

        if (!filter(symbol))
            return;

        try
        {
            builder.Append(new string(' ', indent * 2));
            builder.AppendLine($"{symbol.Kind}: {symbol.Name ?? "<null>"}");

            switch (symbol)
            {
                case IAssemblySymbol asm:
                    AppendSymbol(asm.GlobalNamespace, builder, indent + 1, maxDepth, filter);
                    break;

                case IModuleSymbol mod:
                    AppendSymbol(mod.GlobalNamespace, builder, indent + 1, maxDepth, filter);
                    break;

                case INamespaceSymbol ns:
                    foreach (var member in ns.GetMembers().OrderBy(m => m.Name, StringComparer.Ordinal))
                        AppendSymbol(member, builder, indent + 1, maxDepth, filter);
                    break;

                case INamedTypeSymbol type:
                    foreach (var member in type.GetMembers().OrderBy(m => m.Name, StringComparer.Ordinal))
                        AppendSymbol(member, builder, indent + 1, maxDepth, filter);
                    break;
            }
        }
        catch (InvalidOperationException)
        {
            builder.Append(new string(' ', indent * 2));
            builder.AppendLine($"[Error: Failed to load symbol '{symbol.Name}']");
        }
    }
}