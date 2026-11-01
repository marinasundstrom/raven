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
}
