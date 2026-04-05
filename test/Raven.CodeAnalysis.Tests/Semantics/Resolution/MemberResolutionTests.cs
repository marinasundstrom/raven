using System;

using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class MemberResolutionTests : DiagnosticTestBase
{
    [Fact]
    public void StaticParseResult_MemberAccess_ResolvesMember()
    {
        string testCode =
            """
            import System.*

            val a = DateTime.Parse("2015-03-21").Day;
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void PropertyAccessFollowedByInvocation_ResolvesMember()
    {
        string testCode =
            """
            import System.*

            val a = DateTime.Now.ToString();
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }
}
