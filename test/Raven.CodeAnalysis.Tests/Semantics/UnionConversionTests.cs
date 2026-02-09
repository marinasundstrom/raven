using System.Linq;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
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
        val value = if flag {
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
        val pet = if flag {
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
        var code = "val x: \"true\" | 1 = true";
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
    val value: int = if flag {
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
        var code = "val x: \"true\" | 1 = 2";
        var verifier = CreateVerifier(code, [
            new DiagnosticResult("RAV1504").WithAnySpan().WithArguments("2", "\"true\" | 1")
        ]);
        verifier.Verify();
    }

    [Fact]
    public void NullLiteral_AssignableToUnionParameter()
    {
        var code = """
func describe(input: string | int | null) {}

describe(null)
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }
}

public class UnionConversionClassificationTests : CompilationTestBase
{
    [Fact]
    public void NullLiteral_ClassifyConversionForUnionParameter_IsImplicit()
    {
        var source = """
func describe(input: string | int | null) -> string { "" }

describe(null)
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        var parameterType = symbol.Parameters[0].Type;
        var conversion = compilation.ClassifyConversion(compilation.NullTypeSymbol, parameterType);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
    }
}
