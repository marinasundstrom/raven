using System.Xml;
using System.Xml.Linq;

namespace NodesShared;

public static class Serialization
{
    public static async Task SerializeAsXml(List<SyntaxNodeModel> model, XmlWriter xmlWriter)
    {
        var doc = new XDocument(new XElement("Model", model.Select(n =>
        {
            return new XElement("Node",
                new XAttribute("Name", n.Name),
                OptionalAttributeStr("Inherits", n.Inherits),
                OptionalAttribute<bool>("IsAbstract", n.IsAbstract, false),
                OptionalAttribute<bool>("HasExplicitKind", n.HasExplicitKind, false),
                n.Slots.Select(s =>
                {
                    return new XElement("Slot",
                        new XAttribute("Name", s.Name),
                        new XAttribute("Type", GetType(s.Type)),
                        OptionalAttributeStr("ElementType", GetElementType(s.Type)),
                        OptionalAttribute<bool>("IsNullable", s.IsNullable, false),
                        OptionalAttribute<bool>("IsInherited", s.IsInherited, false),
                        OptionalAttribute<bool>("IsAbstract", s.IsAbstract, false));

                }));
        })));

        await doc.SaveAsync(xmlWriter, default);
    }

    static XAttribute? OptionalAttribute<T>(string name, T? value, T? defaultValue = default)
        where T : struct, IEquatable<T>
    {
        if (value == null || value.Equals(defaultValue))
            return null;
        return new XAttribute(name, value);
    }

    static XAttribute? OptionalAttributeStr(string name, string? value, string? defaultValue = null)
    {
        if (string.IsNullOrEmpty(value) || value == defaultValue)
            return null;
        return new XAttribute(name, value);
    }

    static string GetType(string type)
    {
        if (type.StartsWith("List<"))
        {
            return "List";
        }
        return type;
    }

    static string? GetElementType(string type)
    {
        string? elementType = null;
        int open = type.IndexOf('<');
        int close = type.LastIndexOf('>');

        if (open >= 0 && close > open)
            elementType = type.Substring(open + 1, close - open - 1);

        return elementType;
    }
}
