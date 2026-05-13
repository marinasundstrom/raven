using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class RecordBodyDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void RecordBody_InstanceField_ReportsDiagnostic()
    {
        const string source = """
record class Person(Name: string) {
    private field _age: int = 0
}
""";

        CreateVerifier(
            source,
            [new DiagnosticResult(CompilerDiagnostics.RecordInstanceStorageMemberNotAllowed.Id).WithAnySpan().WithArguments("Person", "field", "_age")])
            .Verify();
    }

    [Fact]
    public void RecordBody_InstanceAutoProperties_ReportDiagnostic()
    {
        const string source = """
record class Person(Name: string) {
    var Age: int = 0
    val Nickname: string { get; }
    val DisplayName: string {
        get => field
    }
}
""";

        CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.RecordInstanceStorageMemberNotAllowed.Id).WithAnySpan().WithArguments("Person", "property", "Age"),
                new DiagnosticResult(CompilerDiagnostics.RecordInstanceStorageMemberNotAllowed.Id).WithAnySpan().WithArguments("Person", "property", "Nickname"),
                new DiagnosticResult(CompilerDiagnostics.RecordInstanceStorageMemberNotAllowed.Id).WithAnySpan().WithArguments("Person", "property", "DisplayName")
            ])
            .Verify();
    }

    [Fact]
    public void RecordBody_InstanceConstructor_ReportsDiagnostic()
    {
        const string source = """
record class Person(Name: string, Age: int) {
    init(name: string) {
    }
}
""";

        CreateVerifier(
            source,
            [new DiagnosticResult(CompilerDiagnostics.RecordSecondaryConstructorNotAllowed.Id).WithAnySpan().WithArguments("Person")])
            .Verify();
    }

    [Fact]
    public void RecordBody_ComputedAndStaticMembers_DoNotReportRecordBodyDiagnostics()
    {
        const string source = """
record class Person(Name: string, Age: int) {
    static field Created: int = 0
    static val EmptyName: string = ""
    const Species: string = "human"

    val IsAdult: bool => Age >= 18
    val Description: string {
        get => "$Name:$Age"
    }

    static func Newborn(name: string) -> Person => Person(name, 0)
    func Rename(name: string) -> Person => Person(name, Age)
    static func +(left: Person, right: Person) -> Person => Person(left.Name + right.Name, left.Age + right.Age)

    class Metadata {}
}
""";

        CreateVerifier(source).Verify();
    }

    [Fact]
    public void RecordStructBody_InstanceField_ReportsDiagnostic()
    {
        const string source = """
record struct Point(X: int, Y: int) {
    field z: int = 0
}
""";

        CreateVerifier(
            source,
            [new DiagnosticResult(CompilerDiagnostics.RecordInstanceStorageMemberNotAllowed.Id).WithAnySpan().WithArguments("Point", "field", "z")])
            .Verify();
    }
}
