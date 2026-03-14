using System.Reflection;
using System.Security.Cryptography;
using System.Xml;
using System.Xml.Linq;

using NodesShared;

using Raven.Generators;

var model = LoadSyntaxNodesFromXml("Model.xml");
var factories = LoadFactoriesFromXml("Factories.xml");
var tokens = LoadTokenKindsFromXml("Tokens.xml");
var nodeKinds = LoadNodeKindsFromXml("NodeKinds.xml");
ValidateModel(model);
ValidateFactoryDefinitions(model, factories);
string hash = await GetHashAsync(model, factories, tokens, nodeKinds);

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

var stats = await GenerateCode(model, factories, tokens, nodeKinds);

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

static async Task GenerateRedNode(
    Dictionary<string, SyntaxNodeModel> nodesByName,
    IReadOnlyDictionary<string, FactoryDefinitionModel> factoriesByNode,
    SyntaxNodeModel node,
    GenerationStats stats)
{
    var source = RedNodeGenerator.GenerateRedNode(node);

    await File.WriteAllTextAsync($"./generated/{node.Name}Syntax.g.cs", source.ToFullString());
    stats.RedNodes++;

    factoriesByNode.TryGetValue(node.Name, out var factoryDefinition);
    var unit = RedNodeGenerator.GenerateRedFactoryMethod(node, factoryDefinition);
    if (unit is not null)
    {
        await File.WriteAllTextAsync($"./generated/{node.Name}Syntax.SyntaxFactory.g.cs", unit.ToFullString());
        stats.RedFactories++;
    }
}

static async Task<string> GetHashAsync(
    List<SyntaxNodeModel> model,
    List<FactoryDefinitionModel> factories,
    List<TokenKindModel> tokens,
    List<NodeKindModel> nodeKinds)
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

    var factoriesRoot = new XElement("Factories");
    foreach (var factory in factories)
    {
        var factoryElement = new XElement(
            "Factory",
            new XAttribute("Node", factory.Node));

        foreach (var overload in factory.Overloads)
        {
            var overloadElement = new XElement("Overload");
            foreach (var parameter in overload.Parameters)
            {
                overloadElement.Add(
                    new XElement(
                        "Parameter",
                        new XAttribute("Slot", parameter.Slot)));
            }

            foreach (var @default in overload.Defaults)
            {
                overloadElement.Add(
                    new XElement(
                        "Default",
                        new XAttribute("Slot", @default.Slot),
                        @default.Token is null ? null : new XAttribute("Token", @default.Token),
                        @default.Null ? new XAttribute("Null", @default.Null) : null));
            }

            factoryElement.Add(overloadElement);
        }

        factoriesRoot.Add(factoryElement);
    }

    var factoriesDoc = new XDocument(factoriesRoot);

    using var tokensStream = new MemoryStream();
    tokensDoc.Save(tokensStream);
    using var nodeKindsStream = new MemoryStream();
    nodeKindsDoc.Save(nodeKindsStream);
    using var factoriesStream = new MemoryStream();
    factoriesDoc.Save(factoriesStream);

    var assemblyBytes = File.ReadAllBytes(Assembly.GetExecutingAssembly().Location);

    var combined = memoryStream.ToArray()
        .Concat(factoriesStream.ToArray())
        .Concat(tokensStream.ToArray())
        .Concat(nodeKindsStream.ToArray())
        .Concat(assemblyBytes)
        .ToArray();

    string hash = Convert.ToHexString(SHA256.HashData(combined));
    return hash;
}

static async Task<GenerationStats> GenerateCode(
    List<SyntaxNodeModel> model,
    List<FactoryDefinitionModel> factories,
    List<TokenKindModel> tokens,
    List<NodeKindModel> nodeKinds)
{
    if (!Directory.Exists("InternalSyntax/generated"))
        Directory.CreateDirectory("InternalSyntax/generated");

    if (!Directory.Exists("generated"))
        Directory.CreateDirectory("generated");

    var stats = new GenerationStats();
    var nodesByName = model.ToDictionary(n => n.Name);
    var factoriesByNode = factories.ToDictionary(f => f.Node);
    foreach (var node in model)
    {
        await GenerateGreenNode(nodesByName, node, stats);
        await GenerateRedNode(nodesByName, factoriesByNode, node, stats);
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
                IsOptionalToken = ParseBool(slotEl.Attribute("IsOptionalToken")),
                IsNullable = ParseBool(slotEl.Attribute("IsNullable")),
                IsInherited = ParseBool(slotEl.Attribute("IsInherited")),
                IsAbstract = ParseBool(slotEl.Attribute("IsAbstract"))
            }).ToList()
        };

        result.Add(node);
    }

    return result;
}

List<FactoryDefinitionModel> LoadFactoriesFromXml(string path)
{
    if (!File.Exists(path))
        return [];

    var doc = XDocument.Load(path);
    var result = new List<FactoryDefinitionModel>();

    foreach (var factoryElement in doc.Descendants("Factory"))
    {
        var factory = new FactoryDefinitionModel
        {
            Node = factoryElement.Attribute("Node")?.Value ?? throw new Exception("Factory missing Node"),
            Overloads = factoryElement.Elements("Overload").Select(overloadElement => new FactoryOverloadModel
            {
                Parameters = overloadElement.Elements("Parameter").Select(parameterElement => new FactoryParameterModel
                {
                    Slot = parameterElement.Attribute("Slot")?.Value ?? throw new Exception("Factory parameter missing Slot")
                }).ToList(),
                Defaults = overloadElement.Elements("Default").Select(defaultElement => new FactoryDefaultModel
                {
                    Slot = defaultElement.Attribute("Slot")?.Value ?? throw new Exception("Factory default missing Slot"),
                    Token = defaultElement.Attribute("Token")?.Value,
                    Null = ParseBool(defaultElement.Attribute("Null"))
                }).ToList()
            }).ToList()
        };

        result.Add(factory);
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
            Value = tokenElement.Attribute("Value")?.Value,
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

void ValidateModel(List<SyntaxNodeModel> model)
{
    var errors = new List<string>();

    foreach (var node in model)
    {
        foreach (var slot in node.Slots)
        {
            if (slot.Type == "Token")
            {
                if (slot.IsNullable)
                {
                    errors.Add(
                        $"Node '{node.Name}', slot '{slot.Name}': token slots cannot use IsNullable=\"true\". Use IsOptionalToken=\"true\" for optional tokens.");
                }
            }
            else
            {
                if (slot.IsOptionalToken)
                {
                    errors.Add(
                        $"Node '{node.Name}', slot '{slot.Name}': IsOptionalToken=\"true\" is only valid for token slots (Type=\"Token\").");
                }

                if (!string.IsNullOrWhiteSpace(slot.DefaultToken))
                {
                    errors.Add(
                        $"Node '{node.Name}', slot '{slot.Name}': DefaultToken is only valid for token slots (Type=\"Token\").");
                }
            }
        }
    }

    if (errors.Count > 0)
    {
        var message =
            "Invalid syntax model configuration detected:\n" +
            string.Join(Environment.NewLine, errors.Select(e => $"  - {e}"));
        throw new InvalidOperationException(message);
    }
}

void ValidateFactoryDefinitions(List<SyntaxNodeModel> model, List<FactoryDefinitionModel> factories)
{
    var errors = new List<string>();
    var nodesByName = model.ToDictionary(n => n.Name);

    foreach (var duplicateGroup in factories.GroupBy(f => f.Node).Where(g => g.Count() > 1))
    {
        errors.Add($"Factory definitions for node '{duplicateGroup.Key}' must be unique.");
    }

    foreach (var factory in factories)
    {
        if (!nodesByName.TryGetValue(factory.Node, out var node))
        {
            errors.Add($"Factory definition references unknown node '{factory.Node}'.");
            continue;
        }

        if (node.IsAbstract)
        {
            errors.Add($"Factory definition for node '{factory.Node}' is invalid because the node is abstract.");
            continue;
        }

        for (var overloadIndex = 0; overloadIndex < factory.Overloads.Count; overloadIndex++)
        {
            var overload = factory.Overloads[overloadIndex];
            var referenced = new HashSet<string>(StringComparer.Ordinal);

            foreach (var parameter in overload.Parameters)
            {
                ValidateFactorySlotReference(node, parameter.Slot, errors, factory.Node, overloadIndex, "parameter");
                if (!referenced.Add(parameter.Slot))
                {
                    errors.Add($"Factory '{factory.Node}' overload {overloadIndex + 1}: slot '{parameter.Slot}' is referenced more than once.");
                }
            }

            foreach (var @default in overload.Defaults)
            {
                ValidateFactorySlotReference(node, @default.Slot, errors, factory.Node, overloadIndex, "default");
                if (!referenced.Add(@default.Slot))
                {
                    errors.Add($"Factory '{factory.Node}' overload {overloadIndex + 1}: slot '{@default.Slot}' is referenced more than once.");
                }

                if (@default.Slot == "Kind")
                {
                    if (@default.Token is null)
                    {
                        errors.Add($"Factory '{factory.Node}' overload {overloadIndex + 1}: explicit kind defaults must use Token=\"...\".");
                    }

                    if (@default.Null)
                    {
                        errors.Add($"Factory '{factory.Node}' overload {overloadIndex + 1}: explicit kind cannot use Null=\"true\".");
                    }

                    continue;
                }

                var slot = node.Slots.Single(s => s.Name == @default.Slot);
                if (@default.Token is not null && slot.Type != "Token")
                {
                    errors.Add($"Factory '{factory.Node}' overload {overloadIndex + 1}: slot '{@default.Slot}' can only use Token=\"...\" when the slot type is Token.");
                }

                if (@default.Null && (!slot.IsNullable && !(slot.Type == "Token" && slot.IsOptionalToken)))
                {
                    errors.Add($"Factory '{factory.Node}' overload {overloadIndex + 1}: slot '{@default.Slot}' can only use Null=\"true\" when the slot is nullable or an optional token.");
                }

                if (@default.Token is null && !@default.Null)
                {
                    errors.Add($"Factory '{factory.Node}' overload {overloadIndex + 1}: default for slot '{@default.Slot}' must specify Token=\"...\" or Null=\"true\".");
                }
            }

            var expected = node.Slots.Count + (node.HasExplicitKind ? 1 : 0);
            if (referenced.Count != expected)
            {
                var missing = new List<string>();
                if (node.HasExplicitKind && !referenced.Contains("Kind"))
                    missing.Add("Kind");
                missing.AddRange(node.Slots.Where(s => !referenced.Contains(s.Name)).Select(s => s.Name));

                errors.Add($"Factory '{factory.Node}' overload {overloadIndex + 1}: every slot must be mapped exactly once. Missing: {string.Join(", ", missing)}.");
            }

            var nonNullBindings = new HashSet<string>(StringComparer.Ordinal);
            foreach (var parameter in overload.Parameters)
                nonNullBindings.Add(parameter.Slot);
            foreach (var @default in overload.Defaults.Where(d => !d.Null))
                nonNullBindings.Add(@default.Slot);

            if (nonNullBindings.Contains("Body") && nonNullBindings.Contains("ExpressionBody"))
            {
                errors.Add($"Factory '{factory.Node}' overload {overloadIndex + 1}: 'Body' and 'ExpressionBody' cannot both be non-null in the same overload.");
            }
        }
    }

    if (errors.Count > 0)
    {
        var message =
            "Invalid syntax factory configuration detected:\n" +
            string.Join(Environment.NewLine, errors.Select(e => $"  - {e}"));
        throw new InvalidOperationException(message);
    }
}

void ValidateFactorySlotReference(
    SyntaxNodeModel node,
    string slotName,
    List<string> errors,
    string factoryNode,
    int overloadIndex,
    string referenceKind)
{
    if (slotName == "Kind")
    {
        if (!node.HasExplicitKind)
        {
            errors.Add($"Factory '{factoryNode}' overload {overloadIndex + 1}: '{referenceKind}' references pseudo-slot 'Kind' but node '{node.Name}' does not have an explicit kind.");
        }

        return;
    }

    if (node.Slots.All(s => s.Name != slotName))
    {
        errors.Add($"Factory '{factoryNode}' overload {overloadIndex + 1}: '{referenceKind}' references unknown slot '{slotName}'.");
    }
}

record TokenKindModel
{
    public required string Name { get; init; }
    public string? Text { get; init; }
    public string? Value { get; init; }
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
