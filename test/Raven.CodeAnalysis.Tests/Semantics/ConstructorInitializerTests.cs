using Raven.CodeAnalysis.Testing;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ConstructorInitializerTests : DiagnosticTestBase
{
    [Fact]
    public void StaticConstructor_WithBaseInitializer_ReportsDiagnostic()
    {
        var code = """
open class Base {
    public init() {}
}

class Derived : Base {
    public static init(): base() {}
}
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics: [
                new DiagnosticResult("RAV0312").WithSpan(6, 27, 6, 31)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ConstructorInitializer_WithNoMatchingOverload_ReportsDiagnostic()
    {
        var code = """
open class Base {
    public init() {}
}

class Derived : Base {
    public init(): base(1) {}
}
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics: [
                new DiagnosticResult("RAV1501").WithSpan(6, 24, 6, 27).WithArguments("Base", 1)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ConstructorInitializer_WithIncompatibleArgumentType_ReportsDiagnostic()
    {
        var code = """
open class Base {
    public init(value: int) {}
}

class Derived : Base {
    public init(): base("text") {}
}
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics: [
                new DiagnosticResult("RAV1503").WithSpan(6, 25, 6, 31).WithArguments("\"text\"", "int")
            ]);

        verifier.Verify();
    }
}
