using System.Security.Cryptography;
using System.Text;
using System.Xml;
using System.Xml.Linq;

using NodesShared;

using Raven.Generators;

var model = LoadSyntaxNodesFromXml("Model.xml");
var tokens = LoadTokenKindsFromXml("Tokens.xml");
var nodeKinds = LoadNodeKindsFromXml("NodeKinds.xml");

string hash = await GetHashAsync(model, tokens, nodeKinds);

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

await GenerateCode(model, tokens, nodeKinds);

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

static async Task<string> GetHashAsync(List<SyntaxNodeModel> model, List<TokenKindModel> tokens, List<NodeKindModel> nodeKinds)
{
    using var memoryStream = new MemoryStream();

    using var xmlWriter = XmlWriter.Create(memoryStream, new XmlWriterSettings { Async = true, Indent = true });

    await Serialization.SerializeAsXml(model, xmlWriter);

    var tokensDoc = new XDocument(new XElement("Tokens", tokens.Select(t =>
        new XElement("TokenKind",
            new XAttribute("Name", t.Name),
            t.Text is null ? null : new XAttribute("Text", t.Text),
            t.IsReservedWord ? new XAttribute("IsReservedWord", t.IsReservedWord) : null,
            t.IsTrivia ? new XAttribute("IsTrivia", t.IsTrivia) : null,
            t.IsUnaryOperator ? new XAttribute("IsUnaryOperator", t.IsUnaryOperator) : null,
            t.IsBinaryOperator ? new XAttribute("IsBinaryOperator", t.IsBinaryOperator) : null,
            t.Precedence is int p ? new XAttribute("Precedence", p) : null))));

    var nodeKindsDoc = new XDocument(new XElement("NodeKinds", nodeKinds.Select(n =>
        new XElement("NodeKind",
            new XAttribute("Name", n.Name),
            new XAttribute("Type", n.Type)))));

    using var tokensStream = new MemoryStream();
    tokensDoc.Save(tokensStream);
    using var nodeKindsStream = new MemoryStream();
    nodeKindsDoc.Save(nodeKindsStream);

    var combined = memoryStream.ToArray().Concat(tokensStream.ToArray()).Concat(nodeKindsStream.ToArray()).ToArray();
    string hash = Convert.ToHexString(SHA256.HashData(combined));
    return hash;
}

static async Task GenerateCode(List<SyntaxNodeModel> model, List<TokenKindModel> tokens, List<NodeKindModel> nodeKinds)
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

    var syntaxKind = SyntaxKindGenerator.Generate(model, nodeKinds, tokens);
    await File.WriteAllTextAsync("./generated/SyntaxKind.g.cs", syntaxKind);

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
            IsReservedWord = ParseBool(tokenElement.Attribute("IsReservedWord")),
            IsTrivia = ParseBool(tokenElement.Attribute("IsTrivia")),
            IsUnaryOperator = ParseBool(tokenElement.Attribute("IsUnaryOperator")),
            IsBinaryOperator = ParseBool(tokenElement.Attribute("IsBinaryOperator")),
            Precedence = int.TryParse(tokenElement.Attribute("Precedence")?.Value, out var prec) ? prec : null
        };

        result.Add(token);
    }

    return result;
}

bool ParseBool(XAttribute? attr, bool defaultValue = false)
{
    return attr != null && bool.TryParse(attr.Value, out var result) ? result : defaultValue;
}

List<NodeKindModel> LoadNodeKindsFromXml(string path)
{
    var doc = XDocument.Load(path);
    var result = new List<NodeKindModel>();

    foreach (var nodeKindElement in doc.Descendants("NodeKind"))
    {
        var nk = new NodeKindModel
        {
            Name = nodeKindElement.Attribute("Name")?.Value ?? throw new Exception("NodeKind missing Name"),
            Type = nodeKindElement.Attribute("Type")?.Value ?? throw new Exception("NodeKind missing Type")
        };
        result.Add(nk);
    }

    return result;
}

record TokenKindModel
{
    public required string Name { get; init; }
    public string? Text { get; init; }
    public bool IsReservedWord { get; init; }
    public bool IsTrivia { get; init; }
    public bool IsUnaryOperator { get; init; }
    public bool IsBinaryOperator { get; init; }
    public int? Precedence { get; init; }
}

record NodeKindModel
{
    public required string Name { get; init; }
    public required string Type { get; init; }
}