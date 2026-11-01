extern alias Seed;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CompilerParity.Tests;

[Trait("Target", "DotNet")]
public sealed class GeneratedSyntaxFactsParityTests
{
    [Fact]
    public void EveryKindHasTheSameClassificationTextAndPrecedence()
    {
        var seedFacts = typeof(Seed::Raven.CodeAnalysis.Syntax.SyntaxFacts);
        var portFacts = typeof(SyntaxFacts);
        Assert.NotSame(seedFacts.Assembly, portFacts.Assembly);
        var classifiers = new[] { "IsKeywordKind", "IsReservedWordKind", "IsUnaryOperatorToken",
            "IsExpression", "IsStatement", "IsLiteralExpression", "IsAssignmentExpression", "IsTypeSyntax",
            "IsOverloadableOperatorToken", "CanBeIdentifier" };
        foreach (var kind in Enum.GetValues<SyntaxKind>().Append((SyntaxKind)(-1)))
        {
            var seedKind = (Seed::Raven.CodeAnalysis.Syntax.SyntaxKind)(int)kind;
            foreach (var classifier in classifiers)
                Assert.Equal(seedFacts.GetMethod(classifier)!.Invoke(null, [seedKind]),
                    portFacts.GetMethod(classifier)!.Invoke(null, [kind]));
            Assert.Equal(Seed::Raven.CodeAnalysis.Syntax.SyntaxFacts.GetSyntaxTokenText(seedKind), kind.GetSyntaxTokenText());
            Assert.Equal(Seed::Raven.CodeAnalysis.Syntax.SyntaxFacts.TryResolveOperatorPrecedence(seedKind, out var expected),
                SyntaxFacts.TryResolveOperatorPrecedence(kind, out var actual));
            Assert.Equal(expected, actual);
        }
    }

    [Fact]
    public void EveryTokenSpellingHasTheSameKeywordAndReservedLookup()
    {
        foreach (var kind in Enum.GetValues<Seed::Raven.CodeAnalysis.Syntax.SyntaxKind>())
        {
            var text = Seed::Raven.CodeAnalysis.Syntax.SyntaxFacts.GetSyntaxTokenText(kind);
            if (text is null)
                continue;
            Assert.Equal(Seed::Raven.CodeAnalysis.Syntax.SyntaxFacts.TryParseKeyword(text, out var keyword),
                SyntaxFacts.TryParseKeyword(text, out var portKeyword));
            Assert.Equal((int)keyword, (int)portKeyword);
            Assert.Equal(Seed::Raven.CodeAnalysis.Syntax.SyntaxFacts.TryParseReservedWord(text, out var reserved),
                SyntaxFacts.TryParseReservedWord(text, out var portReserved));
            Assert.Equal((int)reserved, (int)portReserved);
        }
    }

    [Fact]
    public void EveryUtf16CodeUnitHasTheSameIdentifierClassification()
    {
        for (var value = 0; value <= char.MaxValue; value++)
        {
            var ch = (char)value;
            Assert.Equal(Seed::Raven.CodeAnalysis.Syntax.SyntaxFacts.IsIdentifierStartCharacter(ch),
                SyntaxFacts.IsIdentifierStartCharacter(ch));
            Assert.Equal(Seed::Raven.CodeAnalysis.Syntax.SyntaxFacts.IsIdentifierPartCharacter(ch),
                SyntaxFacts.IsIdentifierPartCharacter(ch));
        }
    }
}
