using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Testing;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class AssignmentExpressionSemanticTests : DiagnosticTestBase
{
    [Fact]
    public void AssignmentExpression_UsedAsValue_IsAllowed()
    {
        const string source = """
var x = 0
var y = (x = 1)
""";

        var verifier = CreateVerifier(source);

        verifier.Verify();
    }

    [Fact]
    public void ImmutableInstanceField_AssignedOutsideConstructor_ReportsDiagnostic()
    {
        const string source = """
class C {
    val value: int
    public init() {
        value = 1
    }

    public f() {
        value = 2
    }
}
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.ReadOnlyFieldCannotBeAssignedTo.Id)
                    .WithSpan(8, 9, 8, 14)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ImmutableInstanceField_AssignedInConstructor_AllowsInitialization()
    {
        const string source = """
class C {
    val value: int

    public init() {
        value = 1
    }
}
""";

        var verifier = CreateVerifier(source);

        verifier.Verify();
    }

    [Fact]
    public void ImmutableStaticField_AssignedInStaticConstructor_AllowsInitialization()
    {
        const string source = """
class C {
    public static val count: int

    static init() {
        count = 1
    }
}
""";

        var verifier = CreateVerifier(source);

        verifier.Verify();
    }

    [Fact]
    public void ImmutableStaticField_AssignedInInstanceConstructor_ReportsDiagnostic()
    {
        const string source = """
class C {
    public static val count: int

    static init() {
        count = 1
    }

    public init() {
        count = 2
    }
}
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.ReadOnlyFieldCannotBeAssignedTo.Id)
                    .WithSpan(9, 9, 9, 14)
            ]);

        verifier.Verify();
    }
}
