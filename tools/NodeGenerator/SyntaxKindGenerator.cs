using System.Collections.Generic;
using System.Text;
using NodesShared;

internal static class SyntaxKindGenerator
{
    public static string Generate(
        IEnumerable<SyntaxNodeModel> nodes,
        IEnumerable<NodeKindModel> nodeKinds,
        IEnumerable<TokenKindModel> tokens)
    {
        var names = new SortedSet<string>(StringComparer.Ordinal)
        {
            "List"
        };

        foreach (var n in nodes)
            names.Add(n.Name);

        foreach (var nk in nodeKinds)
            names.Add(nk.Name);

        foreach (var t in tokens)
            names.Add(t.Name);

        var sb = new StringBuilder();
        sb.AppendLine("namespace Raven.CodeAnalysis.Syntax;");
        sb.AppendLine();
        sb.AppendLine("public enum SyntaxKind");
        sb.AppendLine("{");
        sb.AppendLine("    None = 0,");
        foreach (var name in names)
            sb.AppendLine($"    {name},");
        sb.AppendLine("}");
        return sb.ToString();
    }
}
