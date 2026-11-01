using System.Text;

using NodesShared;

namespace Raven.Generators;

/// <summary>Raven visitor declarations for the existing syntax-node model.</summary>
public static class RavenVisitorGenerator
{
    public static string GenerateVisitor(List<SyntaxNodeModel> nodes, bool generic = false,
        bool isPublic = true, string namespaceName = "Raven.CodeAnalysis.Syntax")
    {
        var builder = new StringBuilder();
        Line(builder, "namespace " + namespaceName);
        Line(builder, "");
        var accessibility = isPublic ? "public" : "internal";
        var parameters = generic ? "<TResult>" : "";
        var result = generic ? " -> TResult" : "";
        Line(builder, $"{accessibility} abstract partial class SyntaxVisitor{parameters} {{");
        var byName = nodes.ToDictionary(n => n.Name, StringComparer.Ordinal);
        foreach (var root in nodes.Where(n => n.IsAbstract))
        {
            var cases = nodes.Where(n => !n.IsAbstract && IsDerivedFrom(n, root.Name, byName))
                .OrderBy(n => n.Name, StringComparer.Ordinal).ToList();
            if (cases.Count == 0)
                continue;
            var name = root.Name;
            Line(builder, $"    virtual func Visit{name}(node: {name}Syntax){result} {{");
            var prefix = generic ? "return " : "";
            Line(builder, $"        {prefix}node match {{");
            foreach (var item in cases)
            {
                var caseName = item.Name;
                Line(builder, $"            {caseName}Syntax n => Visit{caseName}(n),");
            }
            Line(builder, "            _ => DefaultVisit(node),");
            Line(builder, "        }");
            Line(builder, "    }");
        }
        foreach (var node in nodes.Where(n => !n.IsAbstract))
        {
            var name = node.Name;
            Line(builder, $"    virtual func Visit{name}(node: {name}Syntax){result} => DefaultVisit(node)");
        }
        Line(builder, "}");
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

    private static void Line(StringBuilder builder, string line) => builder.AppendLine(line);
}
