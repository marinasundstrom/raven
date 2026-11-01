using System.Text;

using NodesShared;

namespace Raven.Generators;

/// <summary>Raven red-node declarations over the existing green-node model.</summary>
public static partial class RavenRedNodeGenerator
{
    public static string GenerateNode(SyntaxNodeModel node, IReadOnlyList<string> directSubtypes)
    {
        var builder = new StringBuilder();
        Line(builder, "namespace Raven.CodeAnalysis.Syntax");
        Line(builder, "import System.*");
        var name = node.Name + "Syntax";
        if (node.IsAbstract && directSubtypes.Count > 0)
        {
            var types = string.Join(", ", directSubtypes.Select(t => "typeof(" + t + ")"));
            Line(builder, "[System.Runtime.CompilerServices.ClosedHierarchy(" + types + ")]");
        }
        var modifier = node.IsAbstract ? "abstract " : "";
        var baseName = MapType(node.Inherits, false);
        Line(builder, $"public {modifier}partial class {name} : {baseName} {{");
        Line(builder, $"    internal init(green: InternalSyntax.{name}, parent: SyntaxNode?, position: int) : base(green, parent, position) {{ }}");
        var slots = node.Slots.Where(s => !node.IsAbstract || s.IsInherited).ToList();
        if (!node.IsAbstract)
        {
            var parameters = Parameters(node, slots);
            var arguments = new List<string>();
            if (node.HasExplicitKind)
                arguments.Add("kind");
            foreach (var slot in slots)
            {
                var access = Camel(slot.Name) + (slot.IsNullable ? "?.Green" : ".Green");
                var type = GreenType(slot.FullTypeName) + (slot.IsNullable ? "?" : "");
                arguments.Add("(" + type + ")" + access);
            }
            arguments.Add("null");
            arguments.Add("null");
            var greenArguments = string.Join(", ", arguments);
            Line(builder, $"    init({parameters}) : self(InternalSyntax.{name}({greenArguments}), null, 0) {{ }}");
            var updateArguments = string.Join(", ", Arguments(node, slots));
            var comparisons = slots.Select(s => s.Name + " != " + Camel(s.Name)).ToList();
            if (node.HasExplicitKind)
                comparisons.Insert(0, "Kind != kind");
            var condition = comparisons.Count == 0 ? "false" : string.Join(" || ", comparisons);
            Line(builder, $"    func Update({parameters}) -> {name} {{");
            Line(builder, $"        if {condition} {{ return {name}({updateArguments}) }}");
            Line(builder, "        return self");
            Line(builder, "    }");
            foreach (var slot in slots)
            {
                var values = slots.Select(s => s.Name == slot.Name ? Camel(s.Name) : "self." + s.Name).ToList();
                if (node.HasExplicitKind)
                    values.Insert(0, "Kind");
                var valuesText = string.Join(", ", values);
                var parameter = Camel(slot.Name) + ": " + MapType(slot.FullTypeName, slot.IsNullable);
                Line(builder, $"    func With{slot.Name}({parameter}) -> {name} => Update({valuesText})");
            }
            if (node.HasExplicitKind)
            {
                var values = new List<string> { "kind" };
                values.AddRange(slots.Select(s => "self." + s.Name));
                var valuesText = string.Join(", ", values);
                Line(builder, $"    func WithKind(kind: SyntaxKind) -> {name} => Update({valuesText})");
            }
            Line(builder, $"    override func Accept(visitor: SyntaxVisitor) => visitor.Visit{node.Name}(self)");
            Line(builder, $"    override func Accept<T>(visitor: SyntaxVisitor<T>) -> T => visitor.Visit{node.Name}(self)");
        }
        foreach (var slot in node.Slots.Where(s => node.IsAbstract && s.IsAbstract && !s.IsInherited))
            Line(builder, $"    abstract val {slot.Name}: {MapType(slot.FullTypeName, slot.IsNullable)} {{ get; }}");
        var nodeCases = new List<string>();
        for (var index = 0; index < slots.Count; index++)
        {
            var slot = slots[index];
            var rawType = slot.FullTypeName;
            var type = MapType(rawType, slot.IsNullable);
            var requiredType = MapType(rawType, false);
            var propertyModifier = slot.IsInherited ? "override " : "";
            var access = "Green.GetSlot(" + index + ")";
            var position = "Position + Green.GetChildStartPosition(" + index + ")";
            if (rawType == "Token" || rawType == "TokenList")
            {
                var value = requiredType + "((" + GreenType(rawType) + ")" + access + "!, self, " + position + ")";
                if (slot.IsNullable)
                    value = "if " + access + " is null { null } else { " + value + " }";
                Line(builder, $"    {propertyModifier}val {slot.Name}: {type} => {value}");
            }
            else if (IsList(rawType))
            {
                var fieldName = "_" + Camel(slot.Name);
                Line(builder, $"    private field {fieldName}: {requiredType} = default({requiredType})");
                Line(builder, $"    {propertyModifier}val {slot.Name}: {type} {{ get {{");
                if (slot.IsNullable)
                    Line(builder, $"        if {access} is null {{ return null }}");
                Line(builder, $"        if {fieldName} == default({requiredType}) {{ {fieldName} = {requiredType}((InternalSyntax.SyntaxList){access}!, self, {position}) }}");
                Line(builder, $"        return {fieldName}");
                Line(builder, "    } }");
            }
            else
            {
                var fieldName = "_" + Camel(slot.Name);
                Line(builder, $"    private field {fieldName}: {requiredType}? = null");
                var assertion = slot.IsNullable ? "" : "!";
                Line(builder, $"    {propertyModifier}val {slot.Name}: {type} => ({type})GetNodeSlot({index}){assertion}");
                nodeCases.Add($"            {index} => GetRed(ref {fieldName}, {index}),");
            }
        }
        if (nodeCases.Count > 0)
        {
            Line(builder, "    internal override func GetNodeSlot(index: int) -> SyntaxNode? {");
            Line(builder, "        return index match {");
            foreach (var item in nodeCases)
                Line(builder, item);
            Line(builder, "            _ => throw ArgumentOutOfRangeException(nameof(index)),");
            Line(builder, "        }");
            Line(builder, "    }");
        }
        Line(builder, "}");
        return builder.ToString();
    }

    private static string Parameters(SyntaxNodeModel node, List<SlotModel> slots)
    {
        var parameters = slots.Select(s => Camel(s.Name) + ": " + MapType(s.FullTypeName, s.IsNullable)).ToList();
        if (node.HasExplicitKind)
            parameters.Insert(0, "kind: SyntaxKind");
        return string.Join(", ", parameters);
    }

    private static List<string> Arguments(SyntaxNodeModel node, List<SlotModel> slots)
    {
        var arguments = slots.Select(s => Camel(s.Name)).ToList();
        if (node.HasExplicitKind)
            arguments.Insert(0, "kind");
        return arguments;
    }

    private static bool IsList(string type) => type.StartsWith("List<") || type.StartsWith("SeparatedList<");
    private static string GreenType(string type) => "InternalSyntax." + (type == "Node" ? "SyntaxNode" : type == "Token" ? "SyntaxToken" : type == "TokenList" || IsList(type) ? "SyntaxList" : type + "Syntax");
    private static string MapType(string type, bool nullable)
    {
        var mapped = type == "Node" ? "SyntaxNode" : type == "Token" ? "SyntaxToken" : type == "TokenList" ? "SyntaxTokenList" : type + "Syntax";
        if (IsList(type))
        {
            var element = type.Substring(type.IndexOf('<') + 1, type.Length - type.IndexOf('<') - 2);
            mapped = (type.StartsWith("SeparatedList<") ? "SeparatedSyntaxList<" : "SyntaxList<") + element + "Syntax>";
        }
        return nullable ? mapped + "?" : mapped;
    }
    private static string Camel(string name) => char.ToLowerInvariant(name[0]) + name.Substring(1);
    private static void Line(StringBuilder builder, string line) => builder.AppendLine(line);
}
