using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class UnionConversionTests : DiagnosticTestBase
{
    [Fact]
    public void UnionAssignedToObject_ReturnsNoDiagnostic()
    {
        string code = """
import System.*
class Foo {}
class Bar {}

class Baz {
    Test(flag: bool) -> Object {
        if flag {
            return Foo()
        } else {
            return Bar()
        }
    }
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void UnionNotConvertibleToExplicitType_ProducesDiagnostic()
    {
        string code = """
class Foo {}
class Bar {}

class Baz {
    Test(flag: bool) -> Foo {
        if flag {
            return Foo()
        } else {
            return Bar()
        }
    }
}
""";

        var verifier = CreateVerifier(code, [
            new DiagnosticResult("RAV1503").WithSpan(6, 9, 10, 10).WithArguments("Foo | Bar", "Foo"),
            new DiagnosticResult("RAV1503").WithSpan(9, 20, 9, 25).WithArguments("Bar", "Foo"),
        ]);
        verifier.Verify();
    }

    [Fact]
    public void BooleanLiteral_UsesKeywordInDiagnostic()
    {
        var code = "let x: \"true\" | 1 = true";
        var verifier = CreateVerifier(code, [
            new DiagnosticResult("RAV1504").WithAnySpan().WithArguments("true", "\"true\" | 1")
        ]);
        verifier.Verify();
    }

    [Fact]
    public void NumericLiteralNotInUnion_ProducesDiagnostic()
    {
        var code = "let x: \"true\" | 1 = 2";
        var verifier = CreateVerifier(code, [
            new DiagnosticResult("RAV1504").WithAnySpan().WithArguments("2", "\"true\" | 1")
        ]);
        verifier.Verify();
    }
}
