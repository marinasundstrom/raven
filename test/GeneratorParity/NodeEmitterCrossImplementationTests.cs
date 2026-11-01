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
        Assert.Equal(SeedGenerator::Raven.Generators.VisitorGenerator.GenerateVisitorClass(seedNodes, !green, ns).ToFullString(),
            Raven.Generators.VisitorGenerator.GenerateVisitorClass(nodes, !green, ns).ToFullString());
        Assert.Equal(SeedGenerator::Raven.Generators.VisitorGenerator.GenerateGenericVisitorClass(seedNodes, !green, ns).ToFullString(),
            Raven.Generators.VisitorGenerator.GenerateGenericVisitorClass(nodes, !green, ns).ToFullString());
        Assert.Equal(SeedGenerator::Raven.Generators.VisitorGenerator.GenerateSyntaxRewriterClass(seedNodes, !green, ns).ToFullString(),
            Raven.Generators.VisitorGenerator.GenerateSyntaxRewriterClass(nodes, !green, ns).ToFullString());
    }

}
