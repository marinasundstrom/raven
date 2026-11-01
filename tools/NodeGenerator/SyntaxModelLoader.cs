using System.Xml.Linq;

using NodesShared;

namespace Raven.Generators;

internal static class SyntaxModelLoader
{
    public static List<SyntaxNodeModel> LoadSyntaxNodesFromXml(string path)
    {
        var doc = XDocument.Load(path);
        var result = new List<SyntaxNodeModel>();

        foreach (var nodeElement in doc.Descendants(XName.Get("Node")))
        {
            var node = new SyntaxNodeModel
            {
                Name = nodeElement.Attribute(XName.Get("Name"))?.Value ?? throw new Exception("Node missing Name"),
                Inherits = nodeElement.Attribute(XName.Get("Inherits"))?.Value ?? "",
                IsAbstract = ParseBool(nodeElement.Attribute(XName.Get("IsAbstract"))),
                HasExplicitKind = ParseBool(nodeElement.Attribute(XName.Get("HasExplicitKind"))),
                AdditionalPermittedTypes = nodeElement.Elements(XName.Get("PermittedType"))
                    .Select(element => element.Attribute(XName.Get("Name"))?.Value ??
                        throw new Exception("PermittedType missing Name"))
                    .ToList(),
                Slots = nodeElement.Elements(XName.Get("Slot")).Select(slotEl => new SlotModel
                {
                    Name = slotEl.Attribute(XName.Get("Name"))?.Value ?? throw new Exception("Slot missing Name"),
                    Type = slotEl.Attribute(XName.Get("Type"))?.Value ?? throw new Exception("Slot missing Type"),
                    ElementType = slotEl.Attribute(XName.Get("ElementType"))?.Value,
                    DefaultToken = slotEl.Attribute(XName.Get("DefaultToken"))?.Value,
                    IsOptionalToken = ParseBool(slotEl.Attribute(XName.Get("IsOptionalToken"))),
                    IsNullable = ParseBool(slotEl.Attribute(XName.Get("IsNullable"))),
                    IsInherited = ParseBool(slotEl.Attribute(XName.Get("IsInherited"))),
                    IsAbstract = ParseBool(slotEl.Attribute(XName.Get("IsAbstract")))
                }).ToList()
            };

            result.Add(node);
        }

        return result;
    }

    public static List<SyntaxHierarchyModel> LoadSyntaxHierarchiesFromXml(string path)
    {
        var doc = XDocument.Load(path);

        return doc.Descendants(XName.Get("Hierarchy"))
            .Select(hierarchyElement => new SyntaxHierarchyModel
            {
                Name = hierarchyElement.Attribute(XName.Get("Name"))?.Value ??
                    throw new Exception("Hierarchy missing Name"),
                ModelBase = hierarchyElement.Attribute(XName.Get("ModelBase"))?.Value,
                AdditionalPermittedTypes = hierarchyElement.Elements(XName.Get("PermittedType"))
                    .Select(element => element.Attribute(XName.Get("Name"))?.Value ??
                        throw new Exception("PermittedType missing Name"))
                    .ToList()
            })
            .ToList();
    }

    public static List<FactoryDefinitionModel> LoadFactoriesFromXml(string path)
    {
        if (!File.Exists(path))
            return [];

        var doc = XDocument.Load(path);
        var result = new List<FactoryDefinitionModel>();

        foreach (var factoryElement in doc.Descendants(XName.Get("Factory")))
        {
            var factory = new FactoryDefinitionModel
            {
                Node = factoryElement.Attribute(XName.Get("Node"))?.Value ?? throw new Exception("Factory missing Node"),
                Overloads = factoryElement.Elements(XName.Get("Overload")).Select(overloadElement => new FactoryOverloadModel
                {
                    Alias = overloadElement.Attribute(XName.Get("Alias"))?.Value,
                    Summary = overloadElement.Attribute(XName.Get("Summary"))?.Value,
                    Parameters = overloadElement.Elements(XName.Get("Parameter")).Select(parameterElement => new FactoryParameterModel
                    {
                        Slot = parameterElement.Attribute(XName.Get("Slot"))?.Value ?? throw new Exception("Factory parameter missing Slot")
                    }).ToList(),
                    Defaults = overloadElement.Elements(XName.Get("Default")).Select(defaultElement => new FactoryDefaultModel
                    {
                        Slot = defaultElement.Attribute(XName.Get("Slot"))?.Value ?? throw new Exception("Factory default missing Slot"),
                        Token = defaultElement.Attribute(XName.Get("Token"))?.Value,
                        Null = ParseBool(defaultElement.Attribute(XName.Get("Null")))
                    }).ToList()
                }).ToList()
            };

            result.Add(factory);
        }

        return result;
    }

    public static List<TokenKindModel> LoadTokenKindsFromXml(string path)
    {
        var doc = XDocument.Load(path);
        var result = new List<TokenKindModel>();

        foreach (var tokenElement in doc.Descendants(XName.Get("TokenKind")))
        {
            var token = new TokenKindModel(tokenElement.Attribute(XName.Get("Name"))?.Value ?? throw new Exception("TokenKind missing Name"))
            {
                Text = tokenElement.Attribute(XName.Get("Text"))?.Value,
                Value = tokenElement.Attribute(XName.Get("Value"))?.Value,
                IsReservedWord = ParseBool(tokenElement.Attribute(XName.Get("IsReservedWord"))),
                IsTrivia = ParseBool(tokenElement.Attribute(XName.Get("IsTrivia"))),
                IsUnaryOperator = ParseBool(tokenElement.Attribute(XName.Get("IsUnaryOperator"))),
                IsBinaryOperator = ParseBool(tokenElement.Attribute(XName.Get("IsBinaryOperator"))),
                Precedence = int.TryParse(tokenElement.Attribute(XName.Get("Precedence"))?.Value, out var prec) ? prec : null
            };

            result.Add(token);
        }

        return result;
    }

    private static bool ParseBool(XAttribute? attr, bool defaultValue = false)
    {
        return attr is not null && bool.TryParse(attr.Value, out var result) ? result : defaultValue;
    }

    public static List<NodeKindModel> LoadNodeKindsFromXml(string path)
    {
        var doc = XDocument.Load(path);
        var result = new List<NodeKindModel>();

        foreach (var nodeKindElement in doc.Descendants(XName.Get("NodeKind")))
        {
            var nk = new NodeKindModel(
                nodeKindElement.Attribute(XName.Get("Name"))?.Value ?? throw new Exception("NodeKind missing Name"),
                nodeKindElement.Attribute(XName.Get("Type"))?.Value ?? throw new Exception("NodeKind missing Type"));
            result.Add(nk);
        }

        return result;
    }
}
