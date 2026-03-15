using System.Collections.Concurrent;
using System.Reflection;
using System.Text;
using System.Xml.Linq;

using Raven.CodeAnalysis.Documentation;

namespace Raven.CodeAnalysis.Symbols;

internal static class FrameworkXmlDocumentationProvider
{
    private static readonly ConcurrentDictionary<string, Lazy<FrameworkXmlDocumentationFile?>> s_files =
        new(StringComparer.OrdinalIgnoreCase);

    public static DocumentationComment? GetDocumentationComment(PESymbol symbol)
    {
        if (!TryGetMemberId(symbol, out var memberId))
            return null;

        if (!TryGetAssemblyLocation(symbol, out var assemblyLocation))
            return null;

        var file = s_files.GetOrAdd(
            assemblyLocation,
            static path => new Lazy<FrameworkXmlDocumentationFile?>(() => FrameworkXmlDocumentationFile.TryLoad(path))).Value;

        if (file is null || !file.TryGetMember(memberId, out var member))
            return null;

        var markdown = XmlDocumentationMarkdownFormatter.Format(member);
        if (string.IsNullOrWhiteSpace(markdown))
            return null;

        return DocumentationComment.Create(
            DocumentationFormat.Markdown,
            markdown,
            rawText: member.ToString(SaveOptions.DisableFormatting));
    }

    private static bool TryGetAssemblyLocation(PESymbol symbol, out string assemblyLocation)
    {
        assemblyLocation = string.Empty;

        try
        {
            if (symbol.ContainingAssembly is not PEAssemblySymbol peAssembly)
                return false;

            var assembly = peAssembly.GetAssemblyInfo();
            assemblyLocation = assembly.Location;
            return !string.IsNullOrWhiteSpace(assemblyLocation);
        }
        catch
        {
            return false;
        }
    }

    private static bool TryGetMemberId(PESymbol symbol, out string memberId)
    {
        memberId = symbol switch
        {
            PENamedTypeSymbol type => XmlDocumentationCommentIdBuilder.GetTypeMemberId(type.GetTypeInfo().AsType()),
            PEMethodSymbol method => XmlDocumentationCommentIdBuilder.GetMethodMemberId(method.ReflectionMethodBase),
            PEPropertySymbol property => XmlDocumentationCommentIdBuilder.GetPropertyMemberId(property.GetPropertyInfo()),
            PEFieldSymbol field => XmlDocumentationCommentIdBuilder.GetFieldMemberId(field.GetFieldInfo()),
            PEEventSymbol @event => XmlDocumentationCommentIdBuilder.GetEventMemberId(@event.GetEventInfo()),
            _ => string.Empty
        };

        return memberId.Length > 0;
    }
}

internal sealed class FrameworkXmlDocumentationFile
{
    private readonly Dictionary<string, XElement> _members;

    private FrameworkXmlDocumentationFile(Dictionary<string, XElement> members)
    {
        _members = members;
    }

    public static FrameworkXmlDocumentationFile? TryLoad(string assemblyPath)
    {
        var xmlPath = Path.ChangeExtension(assemblyPath, ".xml");
        if (string.IsNullOrWhiteSpace(xmlPath) || !File.Exists(xmlPath))
            return null;

        try
        {
            var document = XDocument.Load(xmlPath, LoadOptions.PreserveWhitespace);
            var members = document.Root?
                .Element("members")?
                .Elements("member")
                .Select(member => (Name: member.Attribute("name")?.Value, Element: member))
                .Where(static item => !string.IsNullOrWhiteSpace(item.Name))
                .ToDictionary(static item => item.Name!, static item => item.Element, StringComparer.Ordinal);

            if (members is null || members.Count == 0)
                return null;

            return new FrameworkXmlDocumentationFile(members);
        }
        catch
        {
            return null;
        }
    }

    public bool TryGetMember(string memberId, out XElement member)
        => _members.TryGetValue(memberId, out member!);
}

internal static class XmlDocumentationCommentIdBuilder
{
    public static string GetTypeMemberId(Type type)
        => "T:" + GetTypeName(type);

    public static string GetMethodMemberId(MethodBase method)
    {
        var declaringType = method.DeclaringType;
        if (declaringType is null)
            return string.Empty;

        var builder = new StringBuilder();
        builder.Append("M:");
        builder.Append(GetTypeName(declaringType));
        builder.Append('.');
        builder.Append(GetMethodName(method));

        AppendParameterList(builder, method.GetParameters().Select(static p => p.ParameterType));

        if (method.Name is "op_Implicit" or "op_Explicit" && method is MethodInfo methodInfo)
        {
            builder.Append('~');
            builder.Append(GetParameterTypeName(methodInfo.ReturnType));
        }

        return builder.ToString();
    }

    public static string GetPropertyMemberId(PropertyInfo property)
    {
        var declaringType = property.DeclaringType;
        if (declaringType is null)
            return string.Empty;

        var builder = new StringBuilder();
        builder.Append("P:");
        builder.Append(GetTypeName(declaringType));
        builder.Append('.');
        builder.Append(property.Name.Replace('.', '#'));
        AppendParameterList(builder, property.GetIndexParameters().Select(static p => p.ParameterType));
        return builder.ToString();
    }

    public static string GetFieldMemberId(FieldInfo field)
    {
        var declaringType = field.DeclaringType;
        if (declaringType is null)
            return string.Empty;

        return $"F:{GetTypeName(declaringType)}.{field.Name.Replace('.', '#')}";
    }

    public static string GetEventMemberId(EventInfo @event)
    {
        var declaringType = @event.DeclaringType;
        if (declaringType is null)
            return string.Empty;

        return $"E:{GetTypeName(declaringType)}.{@event.Name.Replace('.', '#')}";
    }

    private static string GetMethodName(MethodBase method)
    {
        if (method.IsConstructor)
            return method.IsStatic ? "#cctor" : "#ctor";

        var name = method.Name.Replace('.', '#');
        if (method.IsGenericMethod)
            name += "``" + method.GetGenericArguments().Length;

        return name;
    }

    private static void AppendParameterList(StringBuilder builder, IEnumerable<Type> parameterTypes)
    {
        var parameters = parameterTypes.ToArray();
        if (parameters.Length == 0)
            return;

        builder.Append('(');
        for (var i = 0; i < parameters.Length; i++)
        {
            if (i > 0)
                builder.Append(',');

            builder.Append(GetParameterTypeName(parameters[i]));
        }

        builder.Append(')');
    }

    private static string GetTypeName(Type type)
    {
        if (type.IsGenericParameter)
            return type.DeclaringMethod is null
                ? "`" + type.GenericParameterPosition
                : "``" + type.GenericParameterPosition;

        if (type.IsNested && type.DeclaringType is not null)
            return GetTypeName(type.DeclaringType) + "." + GetTypeNameSegment(type, includeGenericArity: true);

        return string.IsNullOrEmpty(type.Namespace)
            ? GetTypeNameSegment(type, includeGenericArity: true)
            : type.Namespace + "." + GetTypeNameSegment(type, includeGenericArity: true);
    }

    private static string GetParameterTypeName(Type type)
    {
        if (type.IsByRef)
            return GetParameterTypeName(type.GetElementType()!) + "@";

        if (type.IsPointer)
            return GetParameterTypeName(type.GetElementType()!) + "*";

        if (type.IsArray)
        {
            var elementType = GetParameterTypeName(type.GetElementType()!);
            if (type.GetArrayRank() == 1)
                return elementType + "[]";

            return elementType + "[" + string.Join(",", Enumerable.Repeat("0:", type.GetArrayRank())) + "]";
        }

        if (type.IsGenericParameter)
            return type.DeclaringMethod is null
                ? "`" + type.GenericParameterPosition
                : "``" + type.GenericParameterPosition;

        if (!type.IsGenericType)
            return GetTypeName(type);

        var genericTypeDefinition = type.GetGenericTypeDefinition();
        var typeArguments = type.GetGenericArguments();
        var formattedArguments = string.Join(",", typeArguments.Select(GetParameterTypeName));
        return $"{GetParameterTypeDefinitionName(genericTypeDefinition)}{{{formattedArguments}}}";
    }

    private static string GetParameterTypeDefinitionName(Type type)
    {
        if (type.IsNested && type.DeclaringType is not null)
            return GetParameterTypeDefinitionName(type.DeclaringType) + "." + GetTypeNameSegment(type, includeGenericArity: false);

        return string.IsNullOrEmpty(type.Namespace)
            ? GetTypeNameSegment(type, includeGenericArity: false)
            : type.Namespace + "." + GetTypeNameSegment(type, includeGenericArity: false);
    }

    private static string GetTypeNameSegment(Type type, bool includeGenericArity)
    {
        var name = type.Name;
        var tickIndex = name.IndexOf('`');
        if (tickIndex >= 0 && !includeGenericArity)
            return name[..tickIndex];

        return name;
    }
}

internal static class XmlDocumentationMarkdownFormatter
{
    public static string Format(XElement member)
    {
        var sections = new List<string>();

        AddIfNotEmpty(sections, RenderBlock(member.Element("summary")));

        var typeParameters = member.Elements("typeparam")
            .Select(param => (Name: param.Attribute("name")?.Value, Text: RenderBlock(param)))
            .Where(static item => !string.IsNullOrWhiteSpace(item.Name) && !string.IsNullOrWhiteSpace(item.Text))
            .Select(static item => $"- `{item.Name}`: {item.Text}")
            .ToArray();
        if (typeParameters.Length > 0)
            sections.Add("**Type Parameters**\n" + string.Join('\n', typeParameters));

        var parameters = member.Elements("param")
            .Select(param => (Name: param.Attribute("name")?.Value, Text: RenderBlock(param)))
            .Where(static item => !string.IsNullOrWhiteSpace(item.Name) && !string.IsNullOrWhiteSpace(item.Text))
            .Select(static item => $"- `{item.Name}`: {item.Text}")
            .ToArray();
        if (parameters.Length > 0)
            sections.Add("**Parameters**\n" + string.Join('\n', parameters));

        AddIfNotEmpty(sections, Prefix("**Returns**", RenderBlock(member.Element("returns"))));
        AddIfNotEmpty(sections, Prefix("**Value**", RenderBlock(member.Element("value"))));
        AddIfNotEmpty(sections, Prefix("**Remarks**", RenderBlock(member.Element("remarks"))));
        AddIfNotEmpty(sections, Prefix("**Example**", RenderBlock(member.Element("example"))));

        var exceptions = member.Elements("exception")
            .Select(ex =>
            {
                var cref = NormalizeCref(ex.Attribute("cref")?.Value);
                var text = RenderBlock(ex);
                return (cref, text);
            })
            .Where(static item => !string.IsNullOrWhiteSpace(item.cref) || !string.IsNullOrWhiteSpace(item.text))
            .Select(static item =>
                string.IsNullOrWhiteSpace(item.cref)
                    ? $"- {item.text}"
                    : $"- `{item.cref}`: {item.text}")
            .ToArray();
        if (exceptions.Length > 0)
            sections.Add("**Exceptions**\n" + string.Join('\n', exceptions));

        if (sections.Count > 0)
            return string.Join("\n\n", sections);

        // Fallback for unexpected XML payloads.
        return NormalizeWhitespace(RenderNodes(member.Nodes()));
    }

    private static string Prefix(string label, string value)
    {
        if (string.IsNullOrWhiteSpace(value))
            return string.Empty;

        return $"{label}\n{value}";
    }

    private static void AddIfNotEmpty(List<string> sections, string value)
    {
        if (!string.IsNullOrWhiteSpace(value))
            sections.Add(value);
    }

    private static string RenderBlock(XElement? element)
    {
        if (element is null)
            return string.Empty;

        return NormalizeWhitespace(RenderNodes(element.Nodes()));
    }

    private static string RenderNodes(IEnumerable<XNode> nodes)
    {
        var builder = new StringBuilder();
        foreach (var node in nodes)
            RenderNode(node, builder);

        return builder.ToString();
    }

    private static void RenderNode(XNode node, StringBuilder builder)
    {
        switch (node)
        {
            case XText text:
                builder.Append(text.Value);
                break;
            case XElement element:
                RenderElement(element, builder);
                break;
        }
    }

    private static void RenderElement(XElement element, StringBuilder builder)
    {
        switch (element.Name.LocalName)
        {
            case "see":
                var cref = NormalizeCref(element.Attribute("cref")?.Value);
                if (!string.IsNullOrWhiteSpace(cref))
                    builder.Append('`').Append(cref).Append('`');
                else if (element.Attribute("langword")?.Value is { Length: > 0 } langword)
                    builder.Append('`').Append(langword).Append('`');
                break;
            case "paramref":
            case "typeparamref":
                if (element.Attribute("name")?.Value is { Length: > 0 } name)
                    builder.Append('`').Append(name).Append('`');
                break;
            case "c":
                builder.Append('`').Append(NormalizeWhitespace(RenderNodes(element.Nodes()))).Append('`');
                break;
            case "code":
                var code = element.Value.Trim();
                if (code.Length > 0)
                    builder.Append("\n```text\n").Append(code).Append("\n```\n");
                break;
            case "para":
                builder.Append('\n').Append('\n');
                builder.Append(NormalizeWhitespace(RenderNodes(element.Nodes())));
                builder.Append('\n').Append('\n');
                break;
            case "br":
                builder.Append('\n');
                break;
            default:
                foreach (var child in element.Nodes())
                    RenderNode(child, builder);
                break;
        }
    }

    private static string NormalizeCref(string? cref)
    {
        if (string.IsNullOrWhiteSpace(cref))
            return string.Empty;

        if (cref.Length > 2 && cref[1] == ':')
            cref = cref.Substring(2);

        return cref.Replace("#ctor", "ctor", StringComparison.Ordinal);
    }

    private static string NormalizeWhitespace(string text)
    {
        if (string.IsNullOrWhiteSpace(text))
            return string.Empty;

        var normalizedNewlines = text.Replace("\r\n", "\n", StringComparison.Ordinal);
        var lines = normalizedNewlines.Split('\n')
            .Select(static line => line.Trim())
            .ToArray();

        // Collapse consecutive blank lines.
        var result = new List<string>(lines.Length);
        var lastBlank = false;
        foreach (var line in lines)
        {
            var isBlank = line.Length == 0;
            if (isBlank && lastBlank)
                continue;

            result.Add(line);
            lastBlank = isBlank;
        }

        return string.Join('\n', result).Trim();
    }
}
