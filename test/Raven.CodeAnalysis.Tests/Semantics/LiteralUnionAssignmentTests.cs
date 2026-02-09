using Raven.CodeAnalysis.Testing;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class LiteralUnionAssignmentTests : DiagnosticTestBase
{
    [Fact]
    public void MatchingValue_AssignsToLiteralUnion()
    {
        var code = "val x: \"true\" | 1 = 1";
        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

}
