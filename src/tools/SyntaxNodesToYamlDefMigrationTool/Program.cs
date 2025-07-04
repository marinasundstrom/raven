using System.ComponentModel;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

using (var file = File.OpenRead("./combined.cs"))
{
    var sourceText = SourceText.From(file);
    var syntaxTree = (CSharpSyntaxTree)CSharpSyntaxTree.ParseText(sourceText);

    var model = new List<SyntaxNodeModel>();

    var root = syntaxTree.GetRoot();

    var classes = root.DescendantNodes()
        .OfType<ClassDeclarationSyntax>()
        .Where(cls => cls.Identifier.Text.EndsWith("Syntax"));

    foreach (var cls in classes)
    {
        var modelEntry = new SyntaxNodeModel
        {
            Name = cls.Identifier.Text.Replace("Syntax", ""),
            Base = cls.BaseList?.Types.FirstOrDefault()?.Type.ToString().Replace("Syntax", "") ?? "Node",
            Abstract = cls.Modifiers.Any(m => m.IsKind(SyntaxKind.AbstractKeyword))
        };

        foreach (var member in cls.Members.OfType<PropertyDeclarationSyntax>())
        {
            if (!member.Modifiers.Any(m => m.IsKind(SyntaxKind.PublicKeyword)))
                continue;

            var propType = member.Type.ToString();
            var isSyntaxLike = propType is "SyntaxToken" or "SyntaxTokenList"
                || propType.EndsWith("Syntax")
                || propType.StartsWith("SyntaxList<")
                || propType.StartsWith("SeparatedSyntaxList<");

            if (!isSyntaxLike)
                continue;

            modelEntry.Properties.Add(new PropertyModel
            {
                Name = member.Identifier.Text,
                Type = propType switch
                {
                    "SyntaxToken" => "Token",
                    "SyntaxTokenList" => "TokenList",
                    var t when t.StartsWith("SyntaxList<") => t.Replace("SyntaxList<", "List<").Replace("Syntax>", ">"),
                    var t when t.StartsWith("SeparatedSyntaxList<") => t.Replace("SeparatedSyntaxList<", "SeparatedList<").Replace("Syntax>", ">"),
                    var t => t.Replace("Syntax", "")
                },
                Nullable = member.Type is NullableTypeSyntax,
                Inherited = member.Modifiers.Any(m => m.IsKind(SyntaxKind.OverrideKeyword)),
                Abstract = member.Modifiers.Any(m => m.IsKind(SyntaxKind.AbstractKeyword))
            });
        }

        model.Add(modelEntry);
    }

    var serializer = new SerializerBuilder()
            .WithNamingConvention(CamelCaseNamingConvention.Instance)
            .ConfigureDefaultValuesHandling(DefaultValuesHandling.OmitDefaults)
            .Build();

    var yaml = serializer.Serialize(model);

    File.WriteAllText("model.yaml", yaml);
    Console.WriteLine(yaml);
}

public class SyntaxNodeModel
{
    public string Name { get; set; } = string.Empty;
    public string Base { get; set; } = string.Empty;

    [DefaultValue(false)]
    public bool Abstract { get; set; }
    public List<PropertyModel> Properties { get; set; } = new();
}

public class PropertyModel
{
    public string Name { get; set; } = string.Empty;
    public string Type { get; set; } = string.Empty;
    public bool Nullable { get; set; }
    public bool Inherited { get; set; }
    public bool Abstract { get; set; }
}