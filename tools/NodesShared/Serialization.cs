using System.Xml;
using System.Xml.Linq;

namespace NodesShared;

public static class Serialization
{
    public static async Task SerializeAsXml(
        List<SyntaxNodeModel> model,
        List<SyntaxHierarchyModel> hierarchies,
        XmlWriter xmlWriter)
    {
        var doc = new XDocument(new XElement(XName.Get("Model"),
            hierarchies.Select(hierarchy =>
                new XElement(XName.Get("Hierarchy"),
                    new XAttribute(XName.Get("Name"), hierarchy.Name),
                    OptionalAttributeStr("ModelBase", hierarchy.ModelBase),
                    hierarchy.AdditionalPermittedTypes.Select(type =>
                        new XElement(XName.Get("PermittedType"), new XAttribute(XName.Get("Name"), type))))),
            model.Select(n =>
            {
                return new XElement(XName.Get("Node"),
                    new XAttribute(XName.Get("Name"), n.Name),
                    OptionalAttributeStr("Inherits", n.Inherits),
                    OptionalAttribute<bool>("IsAbstract", n.IsAbstract, false),
                    OptionalAttribute<bool>("HasExplicitKind", n.HasExplicitKind, false),
                    n.AdditionalPermittedTypes.Select(type =>
                        new XElement(XName.Get("PermittedType"), new XAttribute(XName.Get("Name"), type))),
                    n.Slots.Select(s =>
                    {
                        return new XElement(XName.Get("Slot"),
                            new XAttribute(XName.Get("Name"), s.Name),
                            new XAttribute(XName.Get("Type"), GetType(s.Type)),
                            OptionalAttributeStr("ElementType", s.ElementType ?? GetElementType(s.Type)),
                            OptionalAttributeStr("DefaultToken", s.DefaultToken),
                            OptionalAttribute<bool>("IsOptionalToken", s.IsOptionalToken, false),
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
        if (value is null || EqualityComparer<T?>.Default.Equals(value, defaultValue))
            return null;
        return new XAttribute(XName.Get(name), value!);
    }

    static XAttribute? OptionalAttributeStr(string name, string? value, string? defaultValue = null)
    {
        if (string.IsNullOrEmpty(value) || value == defaultValue)
            return null;
        return new XAttribute(XName.Get(name), value!);
    }

    static string GetType(string type)
    {
        if (type.StartsWith("List<", StringComparison.Ordinal))
        {
            return "List";
        }
        if (type.StartsWith("SeparatedList<", StringComparison.Ordinal))
            return "SeparatedList";
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
