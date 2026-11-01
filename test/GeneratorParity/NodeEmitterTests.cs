using System.Xml.Linq;

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
    internal static List<SyntaxNodeModel> Nodes() => XDocument.Load(Path.Combine(AppContext.BaseDirectory, "Model.xml"))
        .Descendants("Node").Select(n => new SyntaxNodeModel
        {
            Name = (string)n.Attribute("Name")!,
            IsAbstract = (bool?)n.Attribute("IsAbstract") ?? false,
            Inherits = (string?)n.Attribute("Inherits") ?? "",
            HasExplicitKind = (bool?)n.Attribute("HasExplicitKind") ?? false,
            Slots = n.Elements("Slot").Select(s => new SlotModel
            {
                Name = (string)s.Attribute("Name")!,
                Type = (string)s.Attribute("Type")!,
                ElementType = (string?)s.Attribute("ElementType"),
                IsNullable = (bool?)s.Attribute("IsNullable") ?? false,
                IsInherited = (bool?)s.Attribute("IsInherited") ?? false,
                IsAbstract = (bool?)s.Attribute("IsAbstract") ?? false
            }).ToList()
        }).ToList();
    internal static List<NodeKindModel> Kinds() => XDocument.Load(Path.Combine(AppContext.BaseDirectory, "NodeKinds.xml"))
        .Descendants("NodeKind").Select(n => new NodeKindModel((string)n.Attribute("Name")!, (string)n.Attribute("Type")!)).ToList();
    internal static List<TokenKindModel> Tokens() => XDocument.Load(Path.Combine(AppContext.BaseDirectory, "Tokens.xml"))
        .Descendants("TokenKind").Select(n => new TokenKindModel((string)n.Attribute("Name")!)
        {
            Text = (string?)n.Attribute("Text"),
            Value = (string?)n.Attribute("Value"),
            IsReservedWord = (bool?)n.Attribute("IsReservedWord") ?? false,
            IsTrivia = (bool?)n.Attribute("IsTrivia") ?? false,
            IsUnaryOperator = (bool?)n.Attribute("IsUnaryOperator") ?? false,
            IsBinaryOperator = (bool?)n.Attribute("IsBinaryOperator") ?? false,
            Precedence = (int?)n.Attribute("Precedence")
        }).ToList();
}
