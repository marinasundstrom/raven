using System.Security.Cryptography;
using System.Text;
using System.Xml;
using System.Xml.Linq;

using NodesShared;

using Raven.Generators;

var model = LoadSyntaxNodesFromXml("Model.xml");
var tokens = LoadTokenKindsFromXml("Tokens.xml");

string hash = await GetHashAsync(model, tokens);

var force = args.Contains("-f");

// Compare to .stamp
const string stampPath = "./generated/.stamp";
if (File.Exists(stampPath) && File.ReadAllText(stampPath) == hash && !force)
{
    Console.WriteLine("Model unchanged. Skipping generation.");
    return;
}

foreach (var file in Directory.GetFiles("./generated/", "*.g.cs", SearchOption.TopDirectoryOnly))
{
    File.Delete(file);
}

foreach (var file in Directory.GetFiles("./InternalSyntax/generated/", "*.g.cs", SearchOption.TopDirectoryOnly))
{
    File.Delete(file);
}

if (force)
{
    Console.WriteLine("Forcing generation");
}

await GenerateCode(model, tokens);

// Write new hash to .stamp
File.WriteAllText(stampPath, hash);

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

static async Task<string> GetHashAsync(List<SyntaxNodeModel> model, List<TokenKindModel> tokens)
{
    using var memoryStream = new MemoryStream();

    using var xmlWriter = XmlWriter.Create(memoryStream, new XmlWriterSettings { Async = true, Indent = true });

    await Serialization.SerializeAsXml(model, xmlWriter);

    var tokensDoc = new XDocument(new XElement("Tokens", tokens.Select(t =>
        new XElement("TokenKind",
            new XAttribute("Name", t.Name),
            t.Text is null ? null : new XAttribute("Text", t.Text),
            t.IsKeyword ? new XAttribute("IsKeyword", t.IsKeyword) : null,
            t.IsTrivia ? new XAttribute("IsTrivia", t.IsTrivia) : null))));

    using var tokensStream = new MemoryStream();
    tokensDoc.Save(tokensStream);

    var combined = memoryStream.ToArray().Concat(tokensStream.ToArray()).ToArray();
    string hash = Convert.ToHexString(SHA256.HashData(combined));
    return hash;
}

static async Task GenerateCode(List<SyntaxNodeModel> model, List<TokenKindModel> tokens)
{
    if (!Directory.Exists("InternalSyntax/generated"))
        Directory.CreateDirectory("InternalSyntax/generated");

    if (!Directory.Exists("generated"))
        Directory.CreateDirectory("generated");

    var nodesByName = model.ToDictionary(n => n.Name);
    foreach (var node in model)
    {
        await GenerateGreenNode(nodesByName, node);
        await GenerateRedNode(nodesByName, node);
    }

    var internalTokens = TokenGenerator.GenerateInternalFactory(tokens);
    await File.WriteAllTextAsync("./InternalSyntax/generated/SyntaxFactory.Tokens.g.cs", internalTokens);

    var redTokens = TokenGenerator.GenerateRedFactory(tokens);
    await File.WriteAllTextAsync("./generated/SyntaxFactory.Tokens.g.cs", redTokens);

    var factsTokens = TokenGenerator.GenerateSyntaxFacts(tokens);
    await File.WriteAllTextAsync("./generated/SyntaxFacts.Tokens.g.cs", factsTokens);

    var unit = VisitorGenerator.GenerateVisitorClass(model);
    if (unit is not null)
    {
        await File.WriteAllTextAsync($"./generated/SyntaxVisitor.g.cs", unit.ToFullString());
    }

    var unit2 = VisitorGenerator.GenerateVisitorClass(model, false, "Raven.CodeAnalysis.Syntax.InternalSyntax");
    if (unit2 is not null)
    {
        await File.WriteAllTextAsync($"./InternalSyntax/generated/SyntaxVisitor.g.cs", unit2.ToFullString());
    }

    var unit3 = VisitorGenerator.GenerateGenericVisitorClass(model);
    if (unit3 is not null)
    {
        await File.WriteAllTextAsync($"./generated/SyntaxVisitor`1.g.cs", unit3.ToFullString());
    }

    var unit4 = VisitorGenerator.GenerateGenericVisitorClass(model, false, "Raven.CodeAnalysis.Syntax.InternalSyntax");
    if (unit4 is not null)
    {
        await File.WriteAllTextAsync($"./InternalSyntax/generated/SyntaxVisitor`1.g.cs", unit4.ToFullString());
    }

    var unit5 = VisitorGenerator.GenerateSyntaxRewriterClass(model);
    if (unit5 is not null)
    {
        await File.WriteAllTextAsync($"./generated/SyntaxRewriter`1.g.cs", unit5.ToFullString());
    }

    var unit6 = VisitorGenerator.GenerateSyntaxRewriterClass(model, false, "Raven.CodeAnalysis.Syntax.InternalSyntax");
    if (unit6 is not null)
    {
        await File.WriteAllTextAsync($"./InternalSyntax/generated/SyntaxRewriter`1.g.cs", unit6.ToFullString());
    }

    Console.WriteLine($"{model.Count * 2} files were generated for {model.Count} nodes");
}

List<SyntaxNodeModel> LoadSyntaxNodesFromXml(string path)
{
    var doc = XDocument.Load(path);
    var result = new List<SyntaxNodeModel>();

    foreach (var nodeElement in doc.Descendants("Node"))
    {
        var node = new SyntaxNodeModel
        {
            Name = nodeElement.Attribute("Name")?.Value ?? throw new Exception("Node missing Name"),
            Inherits = nodeElement.Attribute("Inherits")?.Value ?? "",
            IsAbstract = ParseBool(nodeElement.Attribute("IsAbstract")),
            HasExplicitKind = ParseBool(nodeElement.Attribute("HasExplicitKind")),
            Slots = nodeElement.Elements("Slot").Select(slotEl => new SlotModel
            {
                Name = slotEl.Attribute("Name")?.Value ?? throw new Exception("Slot missing Name"),
                Type = slotEl.Attribute("Type")?.Value ?? throw new Exception("Slot missing Type"),
                ElementType = slotEl.Attribute("ElementType")?.Value,
                IsNullable = ParseBool(slotEl.Attribute("IsNullable")),
                IsInherited = ParseBool(slotEl.Attribute("IsInherited")),
                IsAbstract = ParseBool(slotEl.Attribute("IsAbstract"))
            }).ToList()
        };

        result.Add(node);
    }

    return result;
}

List<TokenKindModel> LoadTokenKindsFromXml(string path)
{
    var doc = XDocument.Load(path);
    var result = new List<TokenKindModel>();

    foreach (var tokenElement in doc.Descendants("TokenKind"))
    {
        var token = new TokenKindModel
        {
            Name = tokenElement.Attribute("Name")?.Value ?? throw new Exception("TokenKind missing Name"),
            Text = tokenElement.Attribute("Text")?.Value,
            IsKeyword = ParseBool(tokenElement.Attribute("IsKeyword")),
            IsTrivia = ParseBool(tokenElement.Attribute("IsTrivia"))
        };

        result.Add(token);
    }

    return result;
}

bool ParseBool(XAttribute? attr, bool defaultValue = false)
{
    return attr != null && bool.TryParse(attr.Value, out var result) ? result : defaultValue;
}

record TokenKindModel
{
    public required string Name { get; init; }
    public string? Text { get; init; }
    public bool IsKeyword { get; init; }
    public bool IsTrivia { get; init; }
}