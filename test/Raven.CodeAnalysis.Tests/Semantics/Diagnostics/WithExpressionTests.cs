using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class WithExpressionTests : DiagnosticTestBase
{
    [Fact]
    public void WithExpression_UnknownMember_ReportsDiagnostic()
    {
        const string source = """
record class Person(Name: string, Age: int)

val bob = Person("Bob", 30)

val updated = bob with {
    Age = 31
    Unknown = 1
}
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.MemberDoesNotContainDefinition.Id)
                    .WithAnySpan()
                    .WithArguments("Person", "Unknown")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void WithExpression_DuplicateAssignment_ReportsDiagnostic()
    {
        const string source = """
record class Person(Name: string, Age: int)

val bob = Person("Bob", 30)

val updated = bob with {
    Age = 31
    Age = 32
}
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.WithExpressionMemberAssignedMultipleTimes.Id)
                    .WithAnySpan()
                    .WithArguments("Age")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void WithExpression_OnUnsupportedType_ReportsDiagnostic()
    {
        const string source = """
class Person {
    public Name: string { get; set; }
}

val person = Person()
val updated = person with { Name = "Bob" }
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id)
                    .WithAnySpan()
                    .WithArguments("WithName"),
                new DiagnosticResult(CompilerDiagnostics.TypeDoesNotSupportWithExpression.Id)
                    .WithAnySpan()
                    .WithArguments("Person")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void InitOnlyProperty_AssignedOutsideInitializer_ReportsDiagnostic()
    {
        const string source = """
class Person {
    public Name: string { get; init; }

    public init(name: string) {
        Name = name
    }

    public f() {
        Name = "Bob"
    }
}
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.PropertyOrIndexerCannotBeAssignedIsReadOnly.Id)
                    .WithAnySpan()
                    .WithArguments("Name")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void InitOnlyProperty_AssignedInConstructor_ReportsDiagnostic()
    {
        const string source = """
class Person {
    public Name: string { get; init; }

    public init(name: string) {
        Name = name
    }
}
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.PropertyOrIndexerCannotBeAssignedIsReadOnly.Id)
                    .WithAnySpan()
                    .WithArguments("Name")
            ]);
        verifier.Verify();
    }

    [Fact]
    public void WithExpression_AssignsRecordProperties()
    {
        const string source = """
record class Person(Name: string, MiddleName: string?, LastName: string, YearOfBirth: int)

val bob = Person("Bob", null, "Smith", 1978)

val newBob = bob with {
    MiddleName = "Leslie"
}

val renamed = newBob with {
    LastName = "Jones"
}
""";

        var verifier = CreateVerifier(source);
        verifier.Verify();
    }
}
