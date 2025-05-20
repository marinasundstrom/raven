using System.Text;

using Raven.CodeAnalysis;

public static class SymbolExtensions
{
    public static string ToSymbolHierarchyString(this ISymbol symbol, int maxDepth = int.MaxValue)
    {
        var builder = new StringBuilder();
        AppendSymbol(symbol, builder, indent: 0, maxDepth);
        return builder.ToString();
    }

    private static void AppendSymbol(ISymbol symbol, StringBuilder builder, int indent, int maxDepth)
    {
        if (indent > maxDepth)
            return;

        try
        {
            builder.Append(new string(' ', indent * 2));
            builder.AppendLine($"{symbol.Kind}: {symbol.Name ?? "<null>"}");

            switch (symbol)
            {
                case IAssemblySymbol asm:
                    AppendSymbol(asm.GlobalNamespace, builder, indent + 1, maxDepth);
                    break;

                case IModuleSymbol mod:
                    AppendSymbol(mod.GlobalNamespace, builder, indent + 1, maxDepth);
                    break;

                case INamespaceSymbol ns:
                    foreach (var member in ns.GetMembers().OrderBy(m => m.Name, StringComparer.Ordinal))
                        AppendSymbol(member, builder, indent + 1, maxDepth);
                    break;

                case INamedTypeSymbol type:
                    foreach (var member in type.GetMembers().OrderBy(m => m.Name, StringComparer.Ordinal))
                        AppendSymbol(member, builder, indent + 1, maxDepth);
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