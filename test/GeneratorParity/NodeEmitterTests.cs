using Raven.Generators;

using NodesShared;

namespace Raven.GeneratorParity.Tests;

[Trait("Target", "Shared")]
public class NodeEmitterTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void KindsAreOrdinalSortedUniqueAndIncludeList(bool raven)
    {
        var result = SyntaxKindGenerator.Generate([new() { Name = "Zed" }, new() { Name = "Alpha" }],
            [new("Alpha", "Expression")], [new("PlusToken")], raven);
        Assert.Equal($"namespace Raven.CodeAnalysis.Syntax{(raven ? "" : ";")}\n\npublic enum SyntaxKind\n{{\n    None = 0,\n    Alpha,\n    List,\n    PlusToken,\n    Zed,\n}}\n", result.ReplaceLineEndings("\n"));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void FactsUseConcreteKindsAndKeepBinaryAndUnaryBases(bool raven)
    {
        var result = NodeFactsGenerator.GenerateSyntaxFacts([
            new() { Name = "Expression", IsAbstract = true }, new() { Name = "BinaryExpression" },
            new() { Name = "LiteralExpression" }, new() { Name = "Block" }, new() { Name = "IdentifierName" }
        ], [new("StringLiteralExpression", "LiteralExpression"), new("AddExpression", "BinaryExpression")], raven);
        Assert.DoesNotContain("SyntaxKind.Expression =>", result);
        Assert.DoesNotContain("SyntaxKind.LiteralExpression =>", result);
        Assert.Contains("SyntaxKind.BinaryExpression => true", result);
        Assert.Contains("SyntaxKind.StringLiteralExpression => true", result);
        Assert.Contains("SyntaxKind.Block => true", result);
        Assert.Contains("SyntaxKind.IdentifierName => true", result);
        Assert.Contains(raven ? "kind match" : "kind switch", result);
    }

    [Fact]
    public void TokenFactoriesExcludeTriviaAndEscapeLiteralText()
    {
        TokenKindModel[] tokens = [new("Trivia") { Text = " ", IsTrivia = true },
            new("Dynamic"), new("Empty") { Text = "" },
            new("Escaped") { Text = "\"\\\r\n\t" }];
        var green = TokenGenerator.GenerateInternalFactory(tokens);
        var red = TokenGenerator.GenerateRedFactory(tokens);
        Assert.DoesNotContain("SyntaxKind.Trivia", green);
        Assert.DoesNotContain("SyntaxKind.Dynamic", green);
        Assert.Contains("SyntaxKind.Empty, string.Empty", green);
        Assert.Contains("\\\"\\\\\\r\\n\\t", green);
        Assert.Contains("Escaped => Token(SyntaxKind.Escaped)", red);
        Assert.DoesNotContain("SyntaxKind.Trivia", red);
    }

    [Fact]
    public void TokenFactsRetainReservedWordsAndOperatorsInBothLanguages()
    {
        TokenKindModel[] tokens = [new("IfKeyword") { Text = "if", IsReservedWord = true },
            new("PlusToken") { Text = "+", IsBinaryOperator = true, IsUnaryOperator = true, Precedence = 10 }];
        var csharp = TokenGenerator.GenerateSyntaxFacts(tokens);
        var raven = TokenGenerator.GenerateRavenSyntaxFacts(tokens);
        Assert.Contains("precedence = 10;", csharp);
        Assert.Contains("SyntaxKind.IfKeyword", csharp);
        Assert.Contains("SyntaxKind.PlusToken => 10", raven);
        Assert.Contains("CreateReservedWordLookup", raven);
        Assert.Contains("IsUnaryOperatorToken", raven);
    }

    [Fact]
    public void KindModelsPreserveRecordValueEquality()
    {
        var token = new TokenKindModel("PlusToken") { Text = "+", Precedence = 10 };
        var same = new TokenKindModel("PlusToken") { Text = "+", Precedence = 10 };
        Assert.Equal(token, same);
        Assert.Equal(token.GetHashCode(), same.GetHashCode());
        Assert.NotEqual(token, new TokenKindModel("MinusToken") { Text = "-", Precedence = 10 });
        Assert.Equal(new NodeKindModel("Add", "Binary"), new NodeKindModel("Add", "Binary"));
    }
}

internal static class NodeEmitterInputs
{
    internal static List<SyntaxNodeModel> Nodes() => SyntaxModelLoader.LoadSyntaxNodesFromXml(Input("Model.xml"));
    internal static List<SyntaxHierarchyModel> Hierarchies() => SyntaxModelLoader.LoadSyntaxHierarchiesFromXml(Input("Model.xml"));
    internal static List<FactoryDefinitionModel> Factories() => SyntaxModelLoader.LoadFactoriesFromXml(Input("Factories.xml"));
    internal static List<NodeKindModel> Kinds() => SyntaxModelLoader.LoadNodeKindsFromXml(Input("NodeKinds.xml"));
    internal static List<TokenKindModel> Tokens() => SyntaxModelLoader.LoadTokenKindsFromXml(Input("Tokens.xml"));
    private static string Input(string name) => Path.Combine(AppContext.BaseDirectory, name);
}
