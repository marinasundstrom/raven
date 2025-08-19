using System.Text;
using System.Linq;
using NodesShared;

internal static class NodeFactsGenerator
{
    public static string GenerateSyntaxFacts(IEnumerable<SyntaxNodeModel> nodes, IEnumerable<NodeKindModel> nodeKinds)
    {
        var concrete = new HashSet<string>(nodes.Where(n => !n.IsAbstract).Select(n => n.Name));
        foreach (var nk in nodeKinds)
            concrete.Add(nk.Name);

        var baseTypes = nodeKinds.Select(nk => nk.Type).ToHashSet(StringComparer.Ordinal);
        foreach (var b in baseTypes)
        {
            if (b is "UnaryExpression" or "BinaryExpression")
                continue;
            concrete.Remove(b);
        }

        bool IsExpressionKind(string name)
            => name.EndsWith("Expression")
               || (name.EndsWith("Name") && name != "Name" && name != "SimpleName")
               || (name.EndsWith("Type") && name != "Type" && name != "PredefinedType");

        bool IsStatementKind(string name)
            => name.EndsWith("Statement") || name == "Block";

        bool IsLiteralExpressionKind(string name)
            => name.EndsWith("LiteralExpression") && name != "LiteralExpression";

        bool IsAssignmentExpressionKind(string name)
            => name.EndsWith("AssignmentExpression") && name != "AssignmentExpression";

        bool IsTypeSyntaxKind(string name)
            => (name.EndsWith("Type") && name != "Type")
               || (name.EndsWith("Name") && name != "Name" && name != "SimpleName");

        var sorted = concrete.OrderBy(n => n).ToList();
        var expressionKinds = sorted.Where(IsExpressionKind).ToList();
        var statementKinds = sorted.Where(IsStatementKind).ToList();
        var literalKinds = sorted.Where(IsLiteralExpressionKind).ToList();
        var assignmentKinds = sorted.Where(IsAssignmentExpressionKind).ToList();
        var typeKinds = sorted.Where(IsTypeSyntaxKind).ToList();

        var sb = new StringBuilder();
        sb.AppendLine("namespace Raven.CodeAnalysis.Syntax;");
        sb.AppendLine();
        sb.AppendLine("public static partial class SyntaxFacts");
        sb.AppendLine("{");

        void AppendMethod(string methodName, IEnumerable<string> kinds)
        {
            sb.AppendLine($"    public static bool {methodName}(SyntaxKind kind) => kind switch");
            sb.AppendLine("    {");
            foreach (var k in kinds)
                sb.AppendLine($"        SyntaxKind.{k} => true,");
            sb.AppendLine("        _ => false,");
            sb.AppendLine("    };");
            sb.AppendLine();
        }

        AppendMethod("IsExpression", expressionKinds);
        AppendMethod("IsStatement", statementKinds);
        AppendMethod("IsLiteralExpression", literalKinds);
        AppendMethod("IsAssignmentExpression", assignmentKinds);
        AppendMethod("IsTypeSyntax", typeKinds);

        sb.AppendLine("}");
        return sb.ToString();
    }
}

