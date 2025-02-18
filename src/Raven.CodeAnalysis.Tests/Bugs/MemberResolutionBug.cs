using System;

using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Bugs;

public class Issue84_MemberResolutionBug : DiagnosticTestBase
{
    [Fact]
    public void CanResolveMember()
    {
        string testCode =
            """
            import System;

            let a = DateTime.Parse("2015-03-21").Day;
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }
}