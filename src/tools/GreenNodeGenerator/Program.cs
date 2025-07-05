using System.Text;

using Raven.Generators;

using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

using var streamReader = new StreamReader("Model.yaml");

var deserializer = new DeserializerBuilder()
    .WithNamingConvention(CamelCaseNamingConvention.Instance)
    .IgnoreUnmatchedProperties()
    .Build();

var nodes = deserializer.Deserialize<List<SyntaxNodeModel>>(streamReader);

if (!Directory.Exists("InternalSyntax/generated"))
    Directory.CreateDirectory("InternalSyntax/generated");

if (!Directory.Exists("generated"))
    Directory.CreateDirectory("generated");

var nodesByName = nodes.ToDictionary(n => n.Name);
foreach (var node in nodes)
{
    await GenerateGreenNode(nodesByName, node);
    await GenerateRedNode(nodesByName, node);
}

static async Task GenerateGreenNode(Dictionary<string, SyntaxNodeModel> nodesByName, SyntaxNodeModel node)
{
    var source = GreenNodeGenerator.GenerateGreenNode(node, nodesByName);

    await File.WriteAllTextAsync($"./InternalSyntax/generated/{node.Name}Syntax.g.cs", source);

    var unit = GreenNodeGenerator.GenerateFactoryMethod(node);
    if (unit is not null)
    {
        await File.WriteAllTextAsync($"./InternalSyntax/generated/{node.Name}Syntax.SyntaxFactory.g.cs", unit.ToFullString());
    }
}

static async Task GenerateRedNode(Dictionary<string, SyntaxNodeModel> nodesByName, SyntaxNodeModel node)
{
    var source = RedNodeGenerator.GenerateRedNode(node);

    await File.WriteAllTextAsync($"./generated/{node.Name}Syntax.g.cs", source.ToFullString());

    var unit = RedNodeGenerator.GenerateRedFactoryMethod(node);
    if (unit is not null)
    {
        await File.WriteAllTextAsync($"./generated/{node.Name}Syntax.SyntaxFactory.g.cs", unit.ToFullString());
    }
}