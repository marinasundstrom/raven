using System.Linq;
using System.Threading.Tasks;

namespace Raven.CodeAnalysis.Tests.Semantics.Diagnostics;

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
}
