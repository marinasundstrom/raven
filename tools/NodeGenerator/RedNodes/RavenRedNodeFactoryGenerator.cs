using System.Text;

using NodesShared;

namespace Raven.Generators;

public static partial class RavenRedNodeGenerator
{
    public static string? GenerateFactory(SyntaxNodeModel node, FactoryDefinitionModel? definition = null)
    {
        if (node.IsAbstract)
            return null;
        var builder = new StringBuilder();
        Line(builder, "namespace Raven.CodeAnalysis.Syntax");
        Line(builder, "import System.*");
        Line(builder, "public static partial class SyntaxFactory {");
        if (definition is not null)
        {
            foreach (var overload in definition.Overloads)
            {
                var parameters = new List<string>();
                var values = new Dictionary<string, string>(StringComparer.Ordinal);
                foreach (var parameter in overload.Parameters)
                {
                    if (parameter.Slot == "Kind")
                    {
                        parameters.Add("kind: SyntaxKind");
                        values["Kind"] = "kind";
                    }
                    else
                    {
                        var slot = node.Slots.Single(s => s.Name == parameter.Slot);
                        parameters.Add(Camel(slot.Name) + ": " + MapType(slot.FullTypeName, slot.IsNullable));
                        values[slot.Name] = Camel(slot.Name);
                    }
                }
                foreach (var value in overload.Defaults)
                    values[value.Slot] = FactoryDefault(value);
                var arguments = new List<string>();
                if (node.HasExplicitKind)
                    arguments.Add(values["Kind"]);
                arguments.AddRange(node.Slots.Select(s => values[s.Name]));
                if (!string.IsNullOrWhiteSpace(overload.Summary))
                    FactorySummary(builder, overload.Summary!);
                FactoryMethod(builder, node.Name, node.Name + "Syntax", node.Name + "Syntax", parameters, arguments);
                if (!string.IsNullOrWhiteSpace(overload.Alias))
                {
                    FactorySummary(builder, string.IsNullOrWhiteSpace(overload.Summary) ? "Alias for " + node.Name + "." : overload.Summary!);
                    Line(builder, "    /// <remarks>");
                    Line(builder, "    /// Alias for <c>" + node.Name + "</c>. Prefer the canonical factory name unless the alias is clearer at the call site.");
                    Line(builder, "    /// </remarks>");
                    FactoryMethod(builder, overload.Alias!, node.Name + "Syntax", node.Name + "Syntax", parameters, arguments);
                }
            }
        }
        else
        {
            var parameters = new List<string>();
            var minimal = new List<string>();
            var arguments = new List<string>();
            var defaults = new List<string>();
            if (node.HasExplicitKind)
            {
                parameters.Add("kind: SyntaxKind");
                minimal.Add("kind: SyntaxKind");
                arguments.Add("kind");
                defaults.Add("kind");
            }
            foreach (var slot in node.Slots)
            {
                var parameter = Camel(slot.Name) + ": " + MapType(slot.FullTypeName, slot.IsNullable);
                parameters.Add(parameter);
                arguments.Add(Camel(slot.Name));
                if (slot.Type == "Token" && slot.DefaultToken is { } token)
                    defaults.Add(FactoryToken(token));
                else if (slot.Type == "Token" && slot.IsOptionalToken)
                    defaults.Add(FactoryToken("None"));
                else if (slot.IsNullable)
                    defaults.Add("null");
                else
                {
                    minimal.Add(parameter);
                    defaults.Add(Camel(slot.Name));
                }
            }
            if (minimal.Count < parameters.Count)
                FactoryMethod(builder, node.Name, node.Name + "Syntax", node.Name, minimal, defaults);
            FactoryMethod(builder, node.Name, node.Name + "Syntax", node.Name + "Syntax", parameters, arguments);
        }
        Line(builder, "}");
        return builder.ToString();
    }

    private static void FactoryMethod(StringBuilder builder, string name, string typeName, string callee, List<string> parameters, List<string> arguments)
        => Line(builder, "    static func " + name + "(" + string.Join(", ", parameters) + ") -> " +
            typeName + " => " + callee + "(" + string.Join(", ", arguments) + ")");

    private static string FactoryDefault(FactoryDefaultModel value)
    {
        if (value.Token is { } token)
            return FactoryToken(token);
        if (value.Null)
            return "null";
        throw new InvalidOperationException("Factory default for slot '" + value.Slot + "' is missing a supported value.");
    }

    private static string FactoryToken(string token) => "Token(SyntaxKind." + token + ")";

    private static void FactorySummary(StringBuilder builder, string summary)
    {
        Line(builder, "    /// <summary>");
        Line(builder, "    /// " + (System.Security.SecurityElement.Escape(summary) ?? summary));
        Line(builder, "    /// </summary>");
    }
}
