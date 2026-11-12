using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundRewriterSymbolListTests
{
    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(9)]
    public void UnknownLengthSequenceIsVisitedExactlyOnce(int count)
    {
        var original = Make<LocalSymbol>(SymbolKind.Local, "original");
        var replacement = Make<LocalSymbol>(SymbolKind.Local, "replacement");
        var enumerations = 0;
        IEnumerable<Symbol?> Input()
        {
            enumerations++;
            for (var i = 0; i < count; i++)
                yield return original;
        }
        var rewriter = new ReplaceLocal(replacement);
        var result = rewriter.VisitSymbolList<LocalSymbol>(Input());
        Assert.False(result.IsDefault);
        Assert.Equal(count, result.Length);
        Assert.All(result, item => Assert.Same(replacement, item));
        Assert.Equal(count, rewriter.Visits);
        Assert.Equal(1, enumerations);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void KnownAndUnknownLengthInputsPreserveNullSlotsAndOrder(bool iterator)
    {
        var first = Make<LocalSymbol>(SymbolKind.Local, "first");
        var second = Make<LocalSymbol>(SymbolKind.Local, "second");
        Symbol?[] symbols = [first, null, second];
        IEnumerable<Symbol?> Input()
        {
            foreach (var symbol in symbols)
                yield return symbol;
        }
        var result = new Identity().VisitSymbolList<Symbol>(iterator ? Input() : symbols);
        Assert.Equal<Symbol?>(symbols, result);
    }

    private sealed class Identity : BoundTreeRewriter { }

    private sealed class ReplaceLocal(LocalSymbol replacement) : BoundTreeRewriter
    {
        public int Visits { get; private set; }

        public override LocalSymbol VisitLocal(LocalSymbol local)
        {
            Visits++;
            return replacement;
        }
    }
}
