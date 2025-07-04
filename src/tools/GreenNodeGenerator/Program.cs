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

var nodesByName = nodes.ToDictionary(n => n.Name);
foreach (var node in nodes)
{
    var source = GreenNodeGenerator.GenerateGreenNode(node, nodesByName);

    await File.WriteAllTextAsync($"./InternalSyntax/generated/{node.Name}Syntax.g.cs", source);

    var unit = GreenNodeGenerator.GenerateFactoryMethod(node);
    if (unit is not null)
    {
        await File.WriteAllTextAsync($"./InternalSyntax/generated/{node.Name}Syntax.SyntaxFactory.g.cs", unit.ToFullString());
    }
}