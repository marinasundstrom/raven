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

var unit = VisitorGenerator.GenerateVisitorClass(nodes);
if (unit is not null)
{
    await File.WriteAllTextAsync($"./generated/SyntaxVisitor.g.cs", unit.ToFullString());
}

var unit2 = VisitorGenerator.GenerateVisitorClass(nodes, false, "Raven.CodeAnalysis.Syntax.InternalSyntax");
if (unit2 is not null)
{
    await File.WriteAllTextAsync($"./InternalSyntax/generated/SyntaxVisitor.g.cs", unit2.ToFullString());
}

var unit3 = VisitorGenerator.GenerateGenericVisitorClass(nodes);
if (unit3 is not null)
{
    await File.WriteAllTextAsync($"./generated/SyntaxVisitor`1.g.cs", unit3.ToFullString());
}

var unit4 = VisitorGenerator.GenerateGenericVisitorClass(nodes, false, "Raven.CodeAnalysis.Syntax.InternalSyntax");
if (unit4 is not null)
{
    await File.WriteAllTextAsync($"./InternalSyntax/generated/SyntaxVisitor`1.g.cs", unit4.ToFullString());
}

var unit5 = VisitorGenerator.GenerateSyntaxRewriterClass(nodes);
if (unit5 is not null)
{
    await File.WriteAllTextAsync($"./generated/SyntaxRewriter`1.g.cs", unit5.ToFullString());
}

var unit6 = VisitorGenerator.GenerateSyntaxRewriterClass(nodes, false, "Raven.CodeAnalysis.Syntax.InternalSyntax");
if (unit6 is not null)
{
    await File.WriteAllTextAsync($"./InternalSyntax/generated/SyntaxRewriter`1.g.cs", unit6.ToFullString());
}


Console.WriteLine($"{nodes.Count * 2} files were generated for {nodes.Count} nodes");

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