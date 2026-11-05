using System.Linq;
using System.Threading.Tasks;

namespace Raven.CodeAnalysis.Tests.Semantics.Diagnostics;

[Trait("Target", "Shared")]
public sealed class DiagnosticBagConcurrencyTests
{
    private static readonly DiagnosticDescriptor s_descriptor = DiagnosticDescriptor.Create(
        id: "RAVT0001",
        title: "Concurrency test diagnostic",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "diag-{0}",
        category: "Testing",
        defaultSeverity: DiagnosticSeverity.Warning);

    [Fact]
    public void Report_IsStableUnderConcurrentAccess()
    {
        var bag = new DiagnosticBag();
        const int diagnosticCount = 256;

        Parallel.For(0, diagnosticCount, i =>
        {
            bag.Report(Diagnostic.Create(s_descriptor, Location.None, i));
        });

        Assert.Equal(diagnosticCount, bag.AsEnumerable().Count());
    }
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void RepeatedScopeDisposalDoesNotReleaseOtherScopes(bool concurrent)
    {
        var bag = new DiagnosticBag();
        var outer = bag.CreateNonReportingScope();
        var inner = bag.CreateNonReportingScope();
        if (concurrent)
            Parallel.For(0, 64, _ => inner.Dispose());
        else
        {
            inner.Dispose();
            inner.Dispose();
        }
        bag.Report(Diagnostic.Create(s_descriptor, Location.None, "suppressed"));
        Assert.True(bag.IsEmpty);
        outer.Dispose();
        bag.Report(Diagnostic.Create(s_descriptor, Location.None, "accepted"));
        Assert.Single(bag.AsEnumerable());
        using (bag.CreateNonReportingScope())
            bag.Report(Diagnostic.Create(s_descriptor, Location.None, "suppressed-again"));
        Assert.Single(bag.AsEnumerable());
    }

    [Fact]
    public void NestedScopesResumeReportingOnlyAfterBothAreDisposed()
    {
        var bag = new DiagnosticBag();
        var first = bag.CreateNonReportingScope();
        var second = bag.CreateNonReportingScope();
        first.Dispose();
        bag.Report(Diagnostic.Create(s_descriptor, Location.None, "suppressed"));
        Assert.True(bag.IsEmpty);
        second.Dispose();
        bag.Report(Diagnostic.Create(s_descriptor, Location.None, "accepted"));
        Assert.Single(bag.AsEnumerable());
    }
}
