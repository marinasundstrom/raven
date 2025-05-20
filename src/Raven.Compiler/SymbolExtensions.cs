using System.Text;

using Raven.CodeAnalysis;

public static class SymbolExtensions
{
    public static string ToSymbolHierarchyString(this ISymbol symbol)
    {
        var builder = new StringBuilder();
        AppendSymbol(symbol, builder, indent: 0);
        return builder.ToString();
    }

    private static void AppendSymbol(ISymbol symbol, StringBuilder builder, int indent)
    {
        builder.Append(' ', indent * 2);
        builder.AppendLine($"{symbol.ToString()}");

        switch (symbol)
        {
            case INamespaceSymbol ns:
                foreach (var member in ns.GetMembers().OrderBy(m => m.Name))
                {
                    AppendSymbol(member, builder, indent + 1);
                }
                break;

            case INamedTypeSymbol type:
                foreach (var member in type.GetMembers().OrderBy(m => m.Name))
                {
                    AppendSymbol(member, builder, indent + 1);
                }
                break;

            case IAssemblySymbol asm:
                AppendSymbol(asm.GlobalNamespace, builder, indent + 1);
                break;

            case IModuleSymbol mod:
                AppendSymbol(mod.GlobalNamespace, builder, indent + 1);
                break;
        }
    }
}