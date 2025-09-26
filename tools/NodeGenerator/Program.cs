using System.Reflection;
using System.Security.Cryptography;
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
var stampPath = Path.GetFullPath(Path.Combine("generated", ".stamp"));
if (File.Exists(stampPath) && File.ReadAllText(stampPath).Trim() == hash && !force)
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

var stats = await GenerateCode(model, tokens, nodeKinds);

// Write new hash to .stamp
Directory.CreateDirectory(Path.GetDirectoryName(stampPath)!);
File.WriteAllText(stampPath, hash);

Console.WriteLine("Generation summary:");
Console.WriteLine($"  Green nodes: {stats.GreenNodes}");
Console.WriteLine($"  Green factories: {stats.GreenFactories}");
Console.WriteLine($"  Red nodes: {stats.RedNodes}");
Console.WriteLine($"  Red factories: {stats.RedFactories}");
Console.WriteLine($"  Token files: {stats.TokenFiles}");
Console.WriteLine($"  Syntax facts: {stats.SyntaxFacts}");
Console.WriteLine($"  Syntax kind: {stats.SyntaxKind}");
Console.WriteLine($"  Visitor files: {stats.Visitors}");
Console.WriteLine($"  Rewriter files: {stats.Rewriters}");
Console.WriteLine($"Total: {stats.Total}");

static async Task GenerateGreenNode(Dictionary<string, SyntaxNodeModel> nodesByName, SyntaxNodeModel node, GenerationStats stats)
{
    var source = GreenNodeGenerator.GenerateGreenNode(node, nodesByName);

    await File.WriteAllTextAsync($"./InternalSyntax/generated/{node.Name}Syntax.g.cs", source);
    stats.GreenNodes++;

    var unit = GreenNodeGenerator.GenerateFactoryMethod(node);
    if (unit is not null)
    {
        await File.WriteAllTextAsync($"./InternalSyntax/generated/{node.Name}Syntax.SyntaxFactory.g.cs", unit.ToFullString());
        stats.GreenFactories++;
    }
}

static async Task GenerateRedNode(Dictionary<string, SyntaxNodeModel> nodesByName, SyntaxNodeModel node, GenerationStats stats)
{
    var source = RedNodeGenerator.GenerateRedNode(node);

    await File.WriteAllTextAsync($"./generated/{node.Name}Syntax.g.cs", source.ToFullString());
    stats.RedNodes++;

    var unit = RedNodeGenerator.GenerateRedFactoryMethod(node);
    if (unit is not null)
    {
        await File.WriteAllTextAsync($"./generated/{node.Name}Syntax.SyntaxFactory.g.cs", unit.ToFullString());
        stats.RedFactories++;
    }
}

static async Task<string> GetHashAsync(List<SyntaxNodeModel> model, List<TokenKindModel> tokens, List<NodeKindModel> nodeKinds)
{
    using var memoryStream = new MemoryStream();

    await using (var xmlWriter = XmlWriter.Create(memoryStream, new XmlWriterSettings { Async = true, Indent = true }))
    {
        await Serialization.SerializeAsXml(model, xmlWriter);
        await xmlWriter.FlushAsync();
    }

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

    var assemblyBytes = File.ReadAllBytes(Assembly.GetExecutingAssembly().Location);

    var combined = memoryStream.ToArray()
        .Concat(tokensStream.ToArray())
        .Concat(nodeKindsStream.ToArray())
        .Concat(assemblyBytes)
        .ToArray();

    string hash = Convert.ToHexString(SHA256.HashData(combined));
    return hash;
}

static async Task<GenerationStats> GenerateCode(List<SyntaxNodeModel> model, List<TokenKindModel> tokens, List<NodeKindModel> nodeKinds)
{
    if (!Directory.Exists("InternalSyntax/generated"))
        Directory.CreateDirectory("InternalSyntax/generated");

    if (!Directory.Exists("generated"))
        Directory.CreateDirectory("generated");

    var stats = new GenerationStats();
    var nodesByName = model.ToDictionary(n => n.Name);
    foreach (var node in model)
    {
        await GenerateGreenNode(nodesByName, node, stats);
        await GenerateRedNode(nodesByName, node, stats);
    }

    var internalTokens = TokenGenerator.GenerateInternalFactory(tokens);
    await File.WriteAllTextAsync("./InternalSyntax/generated/SyntaxFactory.Tokens.g.cs", internalTokens);
    stats.TokenFiles++;

    var redTokens = TokenGenerator.GenerateRedFactory(tokens);
    await File.WriteAllTextAsync("./generated/SyntaxFactory.Tokens.g.cs", redTokens);
    stats.TokenFiles++;

    var factsTokens = TokenGenerator.GenerateSyntaxFacts(tokens);
    await File.WriteAllTextAsync("./generated/SyntaxFacts.Tokens.g.cs", factsTokens);
    stats.SyntaxFacts++;

    var factsNodes = NodeFactsGenerator.GenerateSyntaxFacts(model, nodeKinds);
    await File.WriteAllTextAsync("./generated/SyntaxFacts.Nodes.g.cs", factsNodes);
    stats.SyntaxFacts++;

    var syntaxKind = SyntaxKindGenerator.Generate(model, nodeKinds, tokens);
    await File.WriteAllTextAsync("./generated/SyntaxKind.g.cs", syntaxKind);
    stats.SyntaxKind++;

    var unit = VisitorGenerator.GenerateVisitorClass(model);
    if (unit is not null)
    {
        await File.WriteAllTextAsync($"./generated/SyntaxVisitor.g.cs", unit.ToFullString());
        stats.Visitors++;
    }

    var unit2 = VisitorGenerator.GenerateVisitorClass(model, false, "Raven.CodeAnalysis.Syntax.InternalSyntax");
    if (unit2 is not null)
    {
        await File.WriteAllTextAsync($"./InternalSyntax/generated/SyntaxVisitor.g.cs", unit2.ToFullString());
        stats.Visitors++;
    }

    var unit3 = VisitorGenerator.GenerateGenericVisitorClass(model);
    if (unit3 is not null)
    {
        await File.WriteAllTextAsync($"./generated/SyntaxVisitor`1.g.cs", unit3.ToFullString());
        stats.Visitors++;
    }

    var unit4 = VisitorGenerator.GenerateGenericVisitorClass(model, false, "Raven.CodeAnalysis.Syntax.InternalSyntax");
    if (unit4 is not null)
    {
        await File.WriteAllTextAsync($"./InternalSyntax/generated/SyntaxVisitor`1.g.cs", unit4.ToFullString());
        stats.Visitors++;
    }

    var unit5 = VisitorGenerator.GenerateSyntaxRewriterClass(model);
    if (unit5 is not null)
    {
        await File.WriteAllTextAsync($"./generated/SyntaxRewriter`1.g.cs", unit5.ToFullString());
        stats.Rewriters++;
    }

    var unit6 = VisitorGenerator.GenerateSyntaxRewriterClass(model, false, "Raven.CodeAnalysis.Syntax.InternalSyntax");
    if (unit6 is not null)
    {
        await File.WriteAllTextAsync($"./InternalSyntax/generated/SyntaxRewriter`1.g.cs", unit6.ToFullString());
        stats.Rewriters++;
    }

    return stats;
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
                DefaultToken = slotEl.Attribute("DefaultToken")?.Value,
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

record GenerationStats
{
    public int GreenNodes;
    public int GreenFactories;
    public int RedNodes;
    public int RedFactories;
    public int TokenFiles;
    public int SyntaxFacts;
    public int SyntaxKind;
    public int Visitors;
    public int Rewriters;
    public int Total => GreenNodes + GreenFactories + RedNodes + RedFactories + TokenFiles + SyntaxFacts + SyntaxKind + Visitors + Rewriters;
}