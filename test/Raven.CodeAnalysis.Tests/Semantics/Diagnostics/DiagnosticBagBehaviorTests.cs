using System.Linq;

using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Semantics.Diagnostics;

[Trait("Target", "Shared")]
public sealed class DiagnosticBagBehaviorTests
{
    private static readonly DiagnosticDescriptor s_descriptor = DiagnosticDescriptor.Create(
        id: "RAVT0002",
        title: "Bag behavior diagnostic",
        description: null,
        helpLinkUri: string.Empty,
        messageFormat: "diag-{0}",
        category: "Testing",
        defaultSeverity: DiagnosticSeverity.Warning);

    [Fact]
    public void EnumerableConstructorPreservesDiagnosticsAndDeduplicatesLaterReports()
    {
        var first = CreateDiagnostic(10, "first");
        var second = CreateDiagnostic(30, "second");
        var bag = new DiagnosticBag([first, second]);

        bag.Report(first);
        bag.Report(CreateDiagnostic(10, "first"));

        Assert.False(bag.IsEmpty);
        Assert.Equal(2, bag.AsEnumerable().Count());
        Assert.Contains(first, bag.AsEnumerable());
        Assert.Contains(second, bag.AsEnumerable());
    }

    [Fact]
    public void ClearDiagnosticsRemovesOnlyIntersectingLocations()
    {
        var first = CreateDiagnostic(10, "first");
        var second = CreateDiagnostic(30, "second");
        var bag = new DiagnosticBag([first, second]);

        bag.ClearDiagnostics(new TextSpan(11, 1));

        Assert.Equal(second, Assert.Single(bag.AsEnumerable()));
        bag.ClearDiagnostics(new TextSpan(31, 1));
        Assert.True(bag.IsEmpty);
        bag.Report(first);
        Assert.Equal(first, Assert.Single(bag.AsEnumerable()));
    }

    [Fact]
    public void ReportingScopesDoNotHideExistingDiagnostics()
    {
        var existing = CreateDiagnostic(10, "existing");
        var bag = new DiagnosticBag([existing]);
        using (bag.CreateNonReportingScope())
        {
            Assert.True(bag.IsReportingDisabled);
            bag.Report(CreateDiagnostic(30, "suppressed"));
            Assert.Equal(existing, Assert.Single(bag.AsEnumerable()));
        }

        Assert.False(bag.IsReportingDisabled);
        bag.Report(CreateDiagnostic(30, "accepted"));
        Assert.Equal(2, bag.AsEnumerable().Count());
    }

    private static Diagnostic CreateDiagnostic(int start, string message)
        => Diagnostic.Create(s_descriptor,
            Location.Create("test.rvn", new TextSpan(start, 3),
                new LinePositionSpan(new LinePosition(0, start), new LinePosition(0, start + 3))), message);
}
