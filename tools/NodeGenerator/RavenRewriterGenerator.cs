using System.Text;

using NodesShared;

namespace Raven.Generators;

/// <summary>Raven rewriting methods paired with generated Raven syntax visitors.</summary>
public static class RavenRewriterGenerator
{
    public static string GenerateRewriter(List<SyntaxNodeModel> nodes, bool isPublic = true,
        string namespaceName = "Raven.CodeAnalysis.Syntax")
    {
        var builder = new StringBuilder();
        var green = namespaceName.Contains("InternalSyntax", StringComparison.Ordinal);
        var accessibility = isPublic ? "public" : "internal";
        builder.AppendLine("namespace " + namespaceName);
        builder.AppendLine($"{accessibility} abstract partial class SyntaxRewriter : SyntaxVisitor<SyntaxNode?> {{");
        var byName = nodes.ToDictionary(n => n.Name, StringComparer.Ordinal);
        foreach (var root in nodes.Where(n => n.IsAbstract))
        {
            var cases = nodes.Where(n => !n.IsAbstract && IsDerivedFrom(n, root.Name, byName))
                .OrderBy(n => n.Name, StringComparer.Ordinal).ToList();
            if (cases.Count == 0)
                continue;
            var name = root.Name;
            builder.AppendLine($"    override func Visit{name}(node: {name}Syntax) -> SyntaxNode? {{");
            builder.AppendLine("        return node match {");
            foreach (var item in cases)
            {
                var caseName = item.Name;
                builder.AppendLine($"            {caseName}Syntax n => Visit{caseName}(n),");
            }
            builder.AppendLine("            _ => node,");
            builder.AppendLine("        }");
            builder.AppendLine("    }");
        }
        foreach (var node in nodes.Where(n => !n.IsAbstract))
        {
            var name = node.Name;
            builder.AppendLine($"    override func Visit{name}(node: {name}Syntax) -> SyntaxNode? {{");
            var arguments = new List<string>();
            if (node.HasExplicitKind)
                arguments.Add("node.Kind");
            foreach (var slot in node.Slots)
            {
                var rawType = slot.FullTypeName;
                var method = rawType == "Node" ? "Visit" : "Visit" + rawType;
                if (rawType == "TokenList")
                    method = green ? "VisitList" : "VisitTokenList";
                else if (rawType.StartsWith("List<") || rawType.StartsWith("SeparatedList<"))
                    method = "VisitList";
                var access = "node." + slot.Name;
                var argument = method + "(" + access + (slot.IsNullable ? "!" : "") + ")";
                if (method != "VisitToken" && method != "VisitList" && method != "VisitTokenList")
                    argument = "(" + MapType(rawType, slot.IsNullable) + ")" + argument;
                if (green && method == "VisitList" && !slot.IsNullable)
                    argument += "!";
                if (slot.IsNullable)
                    argument = "if " + access + " is null { null } else { " + argument + " }";
                arguments.Add(argument);
            }
            var argumentText = string.Join(", ", arguments);
            builder.AppendLine($"        return node.Update({argumentText})");
            builder.AppendLine("    }");
        }
        builder.AppendLine("}");
        return builder.ToString();
    }

    private static bool IsDerivedFrom(SyntaxNodeModel candidate, string rootName,
        IReadOnlyDictionary<string, SyntaxNodeModel> byName)
    {
        var current = candidate;
        while (!string.IsNullOrEmpty(current.Inherits))
        {
            if (string.Equals(current.Inherits, rootName, StringComparison.Ordinal))
                return true;
            if (!byName.TryGetValue(current.Inherits, out var parent))
                break;
            current = parent;
        }
        return false;
    }

    private static string MapType(string rawType, bool nullable)
    {
        var type = rawType == "Node" ? "SyntaxNode" : rawType + "Syntax";
        return nullable ? type + "?" : type;
    }
}
