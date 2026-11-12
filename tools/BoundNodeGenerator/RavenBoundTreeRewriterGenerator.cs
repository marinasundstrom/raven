using System.Text;

namespace BoundNodeGenerator;

static class RavenBoundTreeRewriterGenerator
{
    public static string Generate(IReadOnlyList<BoundNodeModel> models, IEnumerable<string> portedNames)
    {
        var names = portedNames.ToHashSet(StringComparer.Ordinal);
        var byName = models.ToDictionary(node => node.Name, StringComparer.Ordinal);
        var nodes = models.Where(node => names.Contains(node.Name) && !node.IsAbstract)
            .OrderBy(node => node.Name, StringComparer.Ordinal).ToArray();
        bool DerivesFrom(BoundNodeModel node, string root)
        {
            for (var name = node.Name; name is not null; name = byName.TryGetValue(name, out var model) ? model.BaseTypeName : null)
                if (name == root)
                    return true;
            return false;
        }

        var builder = new StringBuilder().AppendLine()
            .AppendLine("internal partial class BoundTreeRewriter {");
        foreach (var category in new[] { "Statement", "Expression", "Pattern", "Designator" })
        {
            var type = "Bound" + category;
            var result = category switch { "Statement" => type, "Expression" => type + "?", _ => "BoundNode" };
            builder.AppendLine($"    override func Visit{category}(node: {type}{(category == "Expression" ? "?" : "")}) -> {result} {{");
            if (category == "Expression")
                builder.AppendLine("        if node is null {\n            return null\n        }");
            builder.AppendLine("        return node match {");
            if (category == "Expression")
            {
                builder.AppendLine("            BoundPattern pattern => (BoundExpression?)VisitPattern(pattern)");
                builder.AppendLine("            BoundDesignator designator => (BoundExpression?)VisitDesignator(designator)");
            }
            foreach (var node in nodes.Where(node => DerivesFrom(node, type) &&
                         (category != "Expression" || !DerivesFrom(node, "BoundPattern") && !DerivesFrom(node, "BoundDesignator"))))
            {
                var call = $"Visit{node.VisitorMethodName}(value)";
                var value = category switch
                {
                    "Statement" => $"(BoundStatement){call}!",
                    "Expression" => $"(BoundExpression?){call}",
                    _ => $"{call} ?? value"
                };
                builder.AppendLine($"            {node.Name} value => {value}");
            }
            builder.AppendLine("            _ => node").AppendLine("        }").AppendLine("    }").AppendLine();
        }
        foreach (var node in nodes.Where(node => node.HasUpdate && node.VisitorMethodName is not ("AssignmentExpression" or "BreakStatement" or "ContinueStatement")))
        {
            builder.AppendLine($"    override func Visit{node.VisitorMethodName}(node: {node.Name}) -> BoundNode? {{");
            var arguments = new List<string>();
            foreach (var parameter in node.Parameters)
            {
                var property = $"node.{parameter.PropertyName}";
                var name = "@" + parameter.ParameterName.TrimStart('@');
                string? value = parameter.Kind switch
                {
                    ParameterKind.BoundNode => $"({parameter.TypeName})Visit({property}){(parameter.IsNullable ? "" : "!")}",
                    ParameterKind.Symbol => $"({parameter.TypeName})VisitSymbol({property}){(parameter.IsNullable ? "" : "!")}",
                    ParameterKind.BoundNodeList => $"VisitList({property}){(parameter.TypeName.StartsWith("ImmutableArray<", StringComparison.Ordinal) ? ".ToImmutableArray()" : "")}",
                    ParameterKind.SymbolList => $"VisitSymbolList<{parameter.ElementTypeName ?? "Symbol"}>({property})",
                    _ => null
                };
                if (value is not null)
                    builder.AppendLine($"        let {name} = {value}");
                arguments.Add(value is null ? property : name);
            }
            builder.AppendLine($"        return node.Update({string.Join(", ", arguments)})").AppendLine("    }").AppendLine();
        }
        return builder.AppendLine("}").ToString();
    }
}
