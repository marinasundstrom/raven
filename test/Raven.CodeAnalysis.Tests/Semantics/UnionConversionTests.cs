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
    public void InstanceMembersFromObjectAreAccessibleOnUnion()
    {
        string code = """
import System.*

class Foo {}
class Bar {}

class Baz {
    Test(flag: bool) {
        let value = if flag {
            Foo()
        } else {
            Bar()
        }

        value.ToString()
    }
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void CommonBaseMembersAreAccessibleOnUnion()
    {
        string code = """
import System.*

open class Animal {
    Speak() -> string {
        "sound"
    }
}

class Dog : Animal {}
class Cat : Animal {}

class Zoo {
    Test(flag: bool) {
        let pet = if flag {
            Dog()
        } else {
            Cat()
        }

        pet.Speak()
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
    public void UnitType_UsesKeywordInDiagnostic()
    {
        var code = """
func test(flag: bool) {
    let value: int = if flag {
        ()
    } else {
        1
    }
}
""";

        var verifier = CreateVerifier(code, [
            new DiagnosticResult("RAV1504").WithAnySpan().WithArguments("unit | int", "int")
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
