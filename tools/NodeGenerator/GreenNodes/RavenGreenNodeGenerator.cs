using System.Text;

using NodesShared;

namespace Raven.Generators;

/// <summary>Raven source output for the same green-node model used by the C# emitter.</summary>
public static class RavenGreenNodeGenerator
{
    public static string GenerateNode(SyntaxNodeModel node)
    {
        var sb = Header();
        var name = node.Name + "Syntax";
        var baseName = MapType(node.Inherits, false);
        var mode = node.IsAbstract ? "abstract" : "open";
        Line(sb, $"internal {mode} partial class {name} : {baseName} {{");
        var slots = node.IsAbstract ? node.Slots.ToList() : node.Slots.Where(s => !s.IsAbstract).ToList();
        if (!node.IsAbstract)
        {
            var parameters = Parameters(node, slots);
            parameters.Add("diagnostics: IEnumerable<DiagnosticInfo>? = null");
            parameters.Add("annotations: IEnumerable<SyntaxAnnotation>? = null");
            var parameterText = string.Join(", ", parameters);
            var kind = node.HasExplicitKind ? "kind" : "SyntaxKind." + node.Name;
            var values = slots.Select(s => s.IsNullable ? Camel(s.Name) : Camel(s.Name) + " ?? throw ArgumentNullException(nameof(" + s.Name + "))");
            var slotText = string.Join(", ", values);
            Line(sb, $"    init({parameterText}) : base({kind}, [{slotText}], diagnostics, annotations) {{ }}");
        }
        Line(sb, "    protected init(kind: SyntaxKind, slots: GreenNode?[], diagnostics: IEnumerable<DiagnosticInfo>? = null, annotations: IEnumerable<SyntaxAnnotation>? = null) : base(kind, slots, diagnostics, annotations) { }");
        var index = 0;
        foreach (var slot in slots)
        {
            var type = MapType(slot.FullTypeName, slot.IsNullable);
            var property = slot.Name;
            if (node.IsAbstract && !slot.IsInherited && slot.IsAbstract)
            {
                Line(sb, $"    abstract val {property}: {type} {{ get; }}");
                continue;
            }
            if (node.IsAbstract && !slot.IsInherited)
                continue;
            var modifier = slot.IsInherited ? "override " : "";
            Line(sb, $"    {modifier}val {property}: {type} => GetSlot<{type}>({index})");
            index++;
        }
        if (!node.IsAbstract)
        {
            var visit = "Visit" + node.Name;
            Line(sb, $"    internal override func Accept(visitor: SyntaxVisitor) {{ visitor.{visit}(self) }}");
            Line(sb, $"    internal override func Accept<TNode>(visitor: SyntaxVisitor<TNode>) -> TNode {{ return visitor.{visit}(self) }}");
            Line(sb, $"    override func CreateRed(parent: Raven.CodeAnalysis.Syntax.SyntaxNode?, position: int) -> Raven.CodeAnalysis.Syntax.SyntaxNode {{ return Raven.CodeAnalysis.Syntax.{name}(self, parent, position) }}");
            Line(sb, $"    internal override func With(children: GreenNode?[], diagnostics: DiagnosticInfo[]? = null, annotations: SyntaxAnnotation[]? = null) -> {name} {{");
            Line(sb, "        var i = 0");
            Line(sb, "        while i < SlotCount {");
            Line(sb, "            if !ReferenceEquals(GetSlot(i), children[i]) { break }");
            Line(sb, "            i++");
            Line(sb, "        }");
            Line(sb, "        if i == SlotCount && AreEqual(diagnostics, _diagnostics) && AreEqual(annotations, _annotations) { return self }");
            Line(sb, $"        return {name}(Kind, children, diagnostics, annotations)");
            Line(sb, "    }");
            var updateParameters = string.Join(", ", Parameters(node, node.Slots));
            var arguments = Arguments(node, node.Slots);
            var updateArguments = string.Join(", ", arguments);
            var conditions = node.Slots.Select(s => "!ReferenceEquals(" + s.Name + ", " + Camel(s.Name) + ")").ToList();
            if (node.HasExplicitKind)
                conditions.Insert(0, "Kind != kind");
            var condition = conditions.Count == 0 ? "false" : string.Join(" || ", conditions);
            Line(sb, $"    func Update({updateParameters}) -> {name} {{");
            Line(sb, $"        if {condition} {{ return {name}({updateArguments}) }}");
            Line(sb, "        return self");
            Line(sb, "    }");
        }
        Line(sb, "}");
        return sb.ToString();
    }

    public static string? GenerateFactory(SyntaxNodeModel node)
    {
        if (node.IsAbstract)
            return null;
        var sb = Header();
        var name = node.Name;
        var type = name + "Syntax";
        var parameters = Parameters(node, node.Slots);
        parameters.Add("diagnostics: IEnumerable<DiagnosticInfo>? = null");
        parameters.Add("annotations: IEnumerable<SyntaxAnnotation>? = null");
        var arguments = Arguments(node, node.Slots);
        arguments.Add("diagnostics");
        arguments.Add("annotations");
        var parameterText = string.Join(", ", parameters);
        var argumentText = string.Join(", ", arguments);
        Line(sb, "internal static partial class SyntaxFactory {");
        if (node.Slots.Any(s => s.IsNullable))
        {
            var minimalParameters = string.Join(", ", Parameters(node, node.Slots.Where(s => !s.IsNullable).ToList()));
            var minimalArguments = new List<string>();
            if (node.HasExplicitKind)
                minimalArguments.Add("kind");
            minimalArguments.AddRange(node.Slots.Select(s => s.IsNullable ? "null" : Camel(s.Name)));
            minimalArguments.Add("null");
            minimalArguments.Add("null");
            var minimalArgumentText = string.Join(", ", minimalArguments);
            Line(sb, $"    static func {name}({minimalParameters}) -> {type} => {name}({minimalArgumentText})");
        }
        Line(sb, $"    static func {name}({parameterText}) -> {type} => {type}({argumentText})");
        Line(sb, "}");
        return sb.ToString();
    }

    private static StringBuilder Header()
    {
        var sb = new StringBuilder();
        Line(sb, "namespace Raven.CodeAnalysis.Syntax.InternalSyntax");
        Line(sb, "");
        Line(sb, "import System.*");
        Line(sb, "import System.Collections.Generic.*");
        Line(sb, "alias SyntaxKind = Raven.CodeAnalysis.Syntax.SyntaxKind");
        Line(sb, "alias SyntaxAnnotation = Raven.CodeAnalysis.Syntax.SyntaxAnnotation");
        Line(sb, "");
        return sb;
    }

    private static List<string> Parameters(SyntaxNodeModel node, List<SlotModel> slots)
    {
        var result = slots.Select(s => Camel(s.Name) + ": " + MapType(s.FullTypeName, s.IsNullable)).ToList();
        if (node.HasExplicitKind)
            result.Insert(0, "kind: SyntaxKind");
        return result;
    }

    private static List<string> Arguments(SyntaxNodeModel node, List<SlotModel> slots)
    {
        var result = slots.Select(s => Camel(s.Name)).ToList();
        if (node.HasExplicitKind)
            result.Insert(0, "kind");
        return result;
    }

    private static string MapType(string rawType, bool nullable)
    {
        var type = rawType switch
        {
            "Node" => "SyntaxNode",
            "Token" => "SyntaxToken",
            "TokenList" => "SyntaxList",
            var value when value.StartsWith("List<") || value.StartsWith("SeparatedList<") => "SyntaxList",
            _ => rawType + "Syntax"
        };
        return nullable ? type + "?" : type;
    }

    private static string Camel(string name) => string.IsNullOrEmpty(name) || char.IsLower(name[0]) ? name : char.ToLowerInvariant(name[0]) + name.Substring(1);
    private static void Line(StringBuilder builder, string line) => builder.AppendLine(line);
}
