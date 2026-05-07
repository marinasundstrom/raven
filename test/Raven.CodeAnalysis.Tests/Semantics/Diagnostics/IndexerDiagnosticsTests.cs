using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class IndexerDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void ElementAccess_WithMismatchedIndexerArgument_ReportsNoMatchingIndexer()
    {
        var code = """
import System.Collections.Generic.*

record JsonObject(Properties: IDictionary<string, int>)

func Main() {
    val x = JsonObject(["name": 1])
    val y = x.Properties[0]
}
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV0022")
                    .WithAnySpan()
                    .WithArguments("IDictionary<string, int>", "int", "'string'")
            ]);

        var result = verifier.GetResult();
        verifier.Verify();

        var diagnostic = Assert.Single(result.MatchedDiagnostics);
        Assert.Equal(
            "No indexer on 'IDictionary<string, int>' accepts argument type(s) 'int'. Available indexer parameter type(s): 'string'.",
            diagnostic.GetMessage());
    }
}
