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
            new DiagnosticResult("RAV1503").WithLocation(6, 9).WithArguments("Foo | Bar", "Foo"),
            new DiagnosticResult("RAV1503").WithLocation(9, 20).WithArguments("Bar", "Foo"),
        ]);
        verifier.Verify();
    }
}
