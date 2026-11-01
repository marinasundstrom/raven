extern alias SeedGenerator;
extern alias SeedModel;

namespace Raven.GeneratorParity.Tests;

[Trait("Target", "DotNet")]
public class NodeEmitterCrossImplementationTests
{
    [Fact]
    public void RealRepositoryInputsProduceIdenticalSourcesInBothImplementations()
    {
        var nodes = NodeEmitterInputs.Nodes();
        var kinds = NodeEmitterInputs.Kinds();
        var tokens = NodeEmitterInputs.Tokens();
        var seedNodes = nodes.Select(n => new SeedModel::NodesShared.SyntaxNodeModel { Name = n.Name, IsAbstract = n.IsAbstract }).ToArray();
        var seedKinds = kinds.Select(n => new SeedGenerator::NodeKindModel(n.Name, n.Type)).ToArray();
        var seedTokens = tokens.Select(t => new SeedGenerator::TokenKindModel(t.Name)
        {
            Text = t.Text,
            Value = t.Value,
            IsTrivia = t.IsTrivia,
            IsReservedWord = t.IsReservedWord,
            IsUnaryOperator = t.IsUnaryOperator,
            IsBinaryOperator = t.IsBinaryOperator,
            Precedence = t.Precedence
        }).ToArray();
        Assert.NotEmpty(nodes);
        Assert.NotEmpty(kinds);
        Assert.NotEmpty(tokens);
        Assert.Equal("NodeGenerator.Raven", typeof(SyntaxKindGenerator).Assembly.GetName().Name);
        Assert.Equal("NodeGenerator.Raven", typeof(NodeFactsGenerator).Assembly.GetName().Name);
        Assert.Equal("NodeGenerator.Raven", typeof(TokenGenerator).Assembly.GetName().Name);
        foreach (var raven in new[] { false, true })
        {
            Assert.Equal(SeedGenerator::SyntaxKindGenerator.Generate(seedNodes, seedKinds, seedTokens, raven), SyntaxKindGenerator.Generate(nodes, kinds, tokens, raven));
            Assert.Equal(SeedGenerator::NodeFactsGenerator.GenerateSyntaxFacts(seedNodes, seedKinds, raven), NodeFactsGenerator.GenerateSyntaxFacts(nodes, kinds, raven));
        }
        Assert.Equal(SeedGenerator::TokenGenerator.GenerateInternalFactory(seedTokens), TokenGenerator.GenerateInternalFactory(tokens));
        Assert.Equal(SeedGenerator::TokenGenerator.GenerateRedFactory(seedTokens), TokenGenerator.GenerateRedFactory(tokens));
        Assert.Equal(SeedGenerator::TokenGenerator.GenerateSyntaxFacts(seedTokens), TokenGenerator.GenerateSyntaxFacts(tokens));
        Assert.Equal(SeedGenerator::TokenGenerator.GenerateRavenSyntaxFacts(seedTokens), TokenGenerator.GenerateRavenSyntaxFacts(tokens));
    }
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void RepositoryVisitorsAndRewritersMatchBothImplementations(bool green)
    {
        var nodes = NodeEmitterInputs.Nodes();
        var seedNodes = nodes.Select(n => new SeedModel::NodesShared.SyntaxNodeModel
        {
            Name = n.Name,
            Inherits = n.Inherits,
            IsAbstract = n.IsAbstract,
            HasExplicitKind = n.HasExplicitKind,
            Slots = n.Slots.Select(s => new SeedModel::NodesShared.SlotModel
            {
                Name = s.Name,
                Type = s.Type,
                ElementType = s.ElementType,
                IsNullable = s.IsNullable
            }).ToList()
        }).ToList();
        var ns = green ? "Raven.CodeAnalysis.Syntax.InternalSyntax" : "Raven.CodeAnalysis.Syntax";
        Assert.Equal("NodeGenerator.Raven", typeof(Raven.Generators.VisitorGenerator).Assembly.GetName().Name);
        Assert.Equal("NodeGenerator.Raven", typeof(Raven.Generators.RavenVisitorGenerator).Assembly.GetName().Name);
        foreach (var generic in new[] { false, true })
            Assert.Equal(SeedGenerator::Raven.Generators.RavenVisitorGenerator.GenerateVisitor(seedNodes, generic, !green, ns),
                Raven.Generators.RavenVisitorGenerator.GenerateVisitor(nodes, generic, !green, ns));
        Assert.Equal(SeedGenerator::Raven.Generators.VisitorGenerator.GenerateVisitorClass(seedNodes, !green, ns).ToFullString(),
            Raven.Generators.VisitorGenerator.GenerateVisitorClass(nodes, !green, ns).ToFullString());
        Assert.Equal(SeedGenerator::Raven.Generators.VisitorGenerator.GenerateGenericVisitorClass(seedNodes, !green, ns).ToFullString(),
            Raven.Generators.VisitorGenerator.GenerateGenericVisitorClass(nodes, !green, ns).ToFullString());
        Assert.Equal(SeedGenerator::Raven.Generators.VisitorGenerator.GenerateSyntaxRewriterClass(seedNodes, !green, ns).ToFullString(),
            Raven.Generators.VisitorGenerator.GenerateSyntaxRewriterClass(nodes, !green, ns).ToFullString());
    }

    [Fact]
    public void RepositoryGreenNodesAndFactoriesMatchBothImplementations()
    {
        var nodes = NodeEmitterInputs.Nodes();
        var seedNodes = nodes.Select(n => new SeedModel::NodesShared.SyntaxNodeModel
        {
            Name = n.Name,
            Inherits = n.Inherits,
            IsAbstract = n.IsAbstract,
            HasExplicitKind = n.HasExplicitKind,
            Slots = n.Slots.Select(s => new SeedModel::NodesShared.SlotModel
            {
                Name = s.Name,
                Type = s.Type,
                ElementType = s.ElementType,
                IsNullable = s.IsNullable,
                IsInherited = s.IsInherited,
                IsAbstract = s.IsAbstract
            }).ToList()
        }).ToList();
        var byName = nodes.ToDictionary(n => n.Name);
        var seedByName = seedNodes.ToDictionary(n => n.Name);
        Assert.NotEmpty(nodes);
        Assert.Equal("NodeGenerator.Raven", typeof(Raven.Generators.GreenNodeGenerator).Assembly.GetName().Name);
        Assert.Equal("NodeGenerator.Raven", typeof(Raven.Generators.RavenGreenNodeGenerator).Assembly.GetName().Name);
        foreach (var node in nodes)
        {
            var seed = seedByName[node.Name];
            Assert.Equal(SeedGenerator::Raven.Generators.RavenGreenNodeGenerator.GenerateNode(seed),
                Raven.Generators.RavenGreenNodeGenerator.GenerateNode(node));
            Assert.Equal(SeedGenerator::Raven.Generators.RavenGreenNodeGenerator.GenerateFactory(seed),
                Raven.Generators.RavenGreenNodeGenerator.GenerateFactory(node));
            Assert.Equal(SeedGenerator::Raven.Generators.GreenNodeGenerator.GenerateGreenNode(seed, seedByName),
                Raven.Generators.GreenNodeGenerator.GenerateGreenNode(node, byName));
            Assert.Equal(SeedGenerator::Raven.Generators.GreenNodeGenerator.GenerateFactoryMethod(seed)?.ToFullString(),
                Raven.Generators.GreenNodeGenerator.GenerateFactoryMethod(node)?.ToFullString());
        }
    }

    [Fact]
    public void RepositoryRedNodesAndFactoriesMatchBothImplementations()
    {
        var nodes = NodeEmitterInputs.Nodes();
        var factories = NodeEmitterInputs.Factories().ToDictionary(f => f.Node);
        Assert.Equal("NodeGenerator.Raven", typeof(Raven.Generators.RedNodeGenerator).Assembly.GetName().Name);
        Assert.NotEmpty(factories);
        foreach (var node in nodes)
        {
            var seed = new SeedModel::NodesShared.SyntaxNodeModel
            {
                Name = node.Name,
                Inherits = node.Inherits,
                IsAbstract = node.IsAbstract,
                HasExplicitKind = node.HasExplicitKind,
                AdditionalPermittedTypes = node.AdditionalPermittedTypes.ToList(),
                Slots = node.Slots.Select(s => new SeedModel::NodesShared.SlotModel
                {
                    Name = s.Name,
                    Type = s.Type,
                    ElementType = s.ElementType,
                    DefaultToken = s.DefaultToken,
                    IsOptionalToken = s.IsOptionalToken,
                    IsNullable = s.IsNullable,
                    IsInherited = s.IsInherited,
                    IsAbstract = s.IsAbstract
                }).ToList()
            };
            var subtypes = nodes.Where(n => n.Inherits == node.Name).Select(n => n.Name + "Syntax")
                .Concat(node.AdditionalPermittedTypes).ToArray();
            Assert.Equal(SeedGenerator::Raven.Generators.RedNodeGenerator.GenerateRedNode(seed, subtypes).ToFullString(),
                Raven.Generators.RedNodeGenerator.GenerateRedNode(node, subtypes).ToFullString());
            Assert.Equal(SeedGenerator::Raven.Generators.RedNodeGenerator.GenerateClosedHierarchyDeclaration(node.Name + "Syntax", subtypes).ToFullString(),
                Raven.Generators.RedNodeGenerator.GenerateClosedHierarchyDeclaration(node.Name + "Syntax", subtypes).ToFullString());
            factories.TryGetValue(node.Name, out var factory);
            var seedFactory = factory is null ? null : new SeedModel::NodesShared.FactoryDefinitionModel
            {
                Node = factory.Node,
                Overloads = factory.Overloads.Select(o => new SeedModel::NodesShared.FactoryOverloadModel
                {
                    Alias = o.Alias,
                    Summary = o.Summary,
                    Parameters = o.Parameters.Select(p => new SeedModel::NodesShared.FactoryParameterModel { Slot = p.Slot }).ToList(),
                    Defaults = o.Defaults.Select(d => new SeedModel::NodesShared.FactoryDefaultModel { Slot = d.Slot, Token = d.Token, Null = d.Null }).ToList()
                }).ToList()
            };
            Assert.Equal(SeedGenerator::Raven.Generators.RedNodeGenerator.GenerateRedFactoryMethod(seed)?.ToFullString(),
                Raven.Generators.RedNodeGenerator.GenerateRedFactoryMethod(node)?.ToFullString());
            Assert.Equal(SeedGenerator::Raven.Generators.RedNodeGenerator.GenerateRedFactoryMethod(seed, seedFactory)?.ToFullString(),
                Raven.Generators.RedNodeGenerator.GenerateRedFactoryMethod(node, factory)?.ToFullString());
        }
    }

    [Theory]
    [InlineData("Escapes <angle> & ampersand.")]
    [InlineData(null)]
    public void FactoryAliasDocumentationMatchesBothImplementations(string? summary)
    {
        var seed = SeedGenerator::Raven.Generators.RedNodeGenerator.GenerateRedFactoryMethod(
            new SeedModel::NodesShared.SyntaxNodeModel { Name = "Empty", Inherits = "Node" },
            new SeedModel::NodesShared.FactoryDefinitionModel { Node = "Empty", Overloads = [new() { Alias = "Alias", Summary = summary }] });
        var translated = Raven.Generators.RedNodeGenerator.GenerateRedFactoryMethod(
            new NodesShared.SyntaxNodeModel { Name = "Empty", Inherits = "Node" },
            new NodesShared.FactoryDefinitionModel { Node = "Empty", Overloads = [new() { Alias = "Alias", Summary = summary }] });
        Assert.Equal(seed!.ToFullString(), translated!.ToFullString());
    }

    [Fact]
    public void RepositoryLoadersPreserveAllModelDataInBothImplementations()
    {
        Assert.Equal("NodeGenerator.Raven", typeof(Raven.Generators.SyntaxModelLoader).Assembly.GetName().Name);
        var modelPath = Path.Combine(AppContext.BaseDirectory, "Model.xml");
        EqualModel(SeedGenerator::Raven.Generators.SyntaxModelLoader.LoadSyntaxNodesFromXml(modelPath), NodeEmitterInputs.Nodes());
        EqualModel(SeedGenerator::Raven.Generators.SyntaxModelLoader.LoadSyntaxHierarchiesFromXml(modelPath), NodeEmitterInputs.Hierarchies());
        EqualModel(SeedGenerator::Raven.Generators.SyntaxModelLoader.LoadFactoriesFromXml(Path.Combine(AppContext.BaseDirectory, "Factories.xml")), NodeEmitterInputs.Factories());
        EqualModel(SeedGenerator::Raven.Generators.SyntaxModelLoader.LoadTokenKindsFromXml(Path.Combine(AppContext.BaseDirectory, "Tokens.xml")), NodeEmitterInputs.Tokens());
        EqualModel(SeedGenerator::Raven.Generators.SyntaxModelLoader.LoadNodeKindsFromXml(Path.Combine(AppContext.BaseDirectory, "NodeKinds.xml")), NodeEmitterInputs.Kinds());
    }

    private static void EqualModel<TSeed, TTranslated>(TSeed seed, TTranslated translated)
    {
        var expected = System.Text.Json.JsonSerializer.SerializeToNode(seed);
        var actual = System.Text.Json.JsonSerializer.SerializeToNode(translated);
        Assert.True(System.Text.Json.Nodes.JsonNode.DeepEquals(expected, actual), $"Expected: {expected}\nActual: {actual}");
    }

}
