using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class RedundantAccessorDeclarationCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void ValProperty_DefaultGetAccessor_RemovesAccessorList()
    {
        const string code = """
class C {
    val Name: string { get; }
}
""";

        const string fixedCode = """
class C {
    val Name: string
}
""";

        var verifier = CreateCodeFixVerifier<RedundantAccessorDeclarationAnalyzer, RedundantAccessorDeclarationCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(RedundantAccessorDeclarationAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void Indexer_DefaultGetSetAccessors_RemovesAccessorList()
    {
        const string code = """
class C {
    var self[index: int]: int { get; set; }
}
""";

        const string fixedCode = """
class C {
    var self[index: int]: int
}
""";

        var verifier = CreateCodeFixVerifier<RedundantAccessorDeclarationAnalyzer, RedundantAccessorDeclarationCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(RedundantAccessorDeclarationAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }
}
