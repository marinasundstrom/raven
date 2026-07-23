using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class MatchStatementDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void MatchStatement_PrefixForm_MissingCoverageReportsExhaustivenessDiagnostic()
    {
        const string code = """
match 1 {
    1 => 1
}
""";

        var verifier = CreateVerifier(
            code,
            [
                new DiagnosticResult("RAV2100").WithAnySpan().WithArguments("..0"),
                new DiagnosticResult("RAV2100").WithAnySpan().WithArguments("2.."),
            ]);

        verifier.Verify();
    }

    [Fact]
    public void MatchStatement_PrefixForm_MissingCoverageReportsExhaustivenessDiagnosticAtMatchKeyword()
    {
        const string code = """
match 1 {
    1 => 1
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "match_statement_missing_coverage_location",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics().Where(d => d.Descriptor.Id == "RAV2100").ToArray();
        var statement = tree.GetRoot().DescendantNodes().OfType<MatchStatementSyntax>().Single();

        Assert.Collection(
            diagnostics,
            diagnostic => Assert.Equal(statement.MatchKeyword.GetLocation(), diagnostic.Location),
            diagnostic => Assert.Equal(statement.MatchKeyword.GetLocation(), diagnostic.Location));
    }

    [Fact]
    public void MatchStatement_WithEnumScrutinee_MissingCoverage_IsReported()
    {
        const string code = """
class Program {
    func eval(color: Color) -> int {
        match color {
            .Red => 1
        }
    }
}

enum Color {
    Red
    Blue
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "match_statement_enum_missing_coverage_location",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        compilation.EnsureSetup();
        Assert.Contains(compilation.GetDiagnostics(), d => d.Descriptor.Id == "RAV2100");
    }

    [Fact]
    public void MatchStatement_WithStructUnionDefaultLocal_AllCasesCoveredIsSourceExhaustive()
    {
        const string code = """
union State {
    case On
    case Off
}

val state: State = default

match state {
    .On => 1
    .Off => 0
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchStatement_WithStructUnionDefaultLocal_MissingSemanticCaseIsReported()
    {
        const string code = """
union State {
    case On
    case Off
}

val state: State = default

match state {
    .On => 1
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2100").WithAnySpan().WithArguments("Off")]);

        verifier.Verify();
    }

    [Fact]
    public void MatchStatement_WithActiveStructUnionScrutinee_DefensiveCatchAllIsRedundant()
    {
        const string code = """
union State {
    case On
    case Off
}

val state: State = .On

match state {
    .On => 1
    .Off => 0
    _ => -1
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2103").WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void MatchStatement_WithStructUnionDefaultLocal_CatchAllForDefaultIsNotRedundant()
    {
        const string code = """
union State {
    case On
    case Off
}

val state: State = default

match state {
    .On => 1
    .Off => 0
    _ => -1
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchStatement_WithStructUnionParameter_AllCasesCoveredIsSourceExhaustive()
    {
        const string code = """
union State {
    case On
    case Off
}

func eval(state: State) -> int {
    match state {
        .On => 1
        .Off => 0
    }
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchStatement_WithStructUnionParameter_DefensiveCatchAllIsRedundant()
    {
        const string code = """
union State {
    case On
    case Off
}

func eval(state: State) -> int {
    match state {
        .On => 1
        .Off => 0
        _ => -1
    }
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2103").WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void MatchStatement_WithStructUnionSelf_DefensiveCatchAllIsRedundant()
    {
        const string code = """
union State {
    case On
    case Off

    func eval() -> int {
        self match {
            On => 1
            Off => 0
            _ => -1
        }
    }
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2103").WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void MatchStatement_WithDiscriminatedUnionScrutinee_RedundantCatchAllReportsDiagnosticAtCatchAllPattern()
    {
        const string code = """
val result: Result<int> = .Ok(value: 1)

match result {
    .Ok(val payload) => payload
    .Error(val message) => 0
    _ => -1
}

union Result<T> {
    case Ok(value: T)
    case Error(message: string)
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "match_statement_du_redundant_catch_all_location",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        compilation.EnsureSetup();
        var diagnostic = Assert.Single(compilation.GetDiagnostics().Where(d => d.Descriptor.Id == "RAV2103"));
        var statement = tree.GetRoot().DescendantNodes().OfType<MatchStatementSyntax>().Single();

        Assert.Equal(statement.Arms[2].Pattern.GetLocation(), diagnostic.Location);
    }

    [Fact]
    public void MatchStatement_UserDefinedUnionCasesRequireQualificationOrImport()
    {
        const string code = """
val s = Status.Open("foo")

match s {
    Open(_) => ""
    Closed(_) => ""
}

union Status {
    case Closed(reason: string)
    case Open(reason: string)
}
""";

        var verifier = CreateVerifier(
            code,
            [
                new DiagnosticResult(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id)
                    .WithAnySpan()
                    .WithArguments("Open"),
                new DiagnosticResult(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id)
                    .WithAnySpan()
                    .WithArguments("Closed"),
                new DiagnosticResult("RAV2101").WithAnySpan(),
            ]);

        verifier.Verify();
    }

    [Fact]
    public void MatchStatement_UserDefinedUnionCasesCanBindFromWildcardImport()
    {
        const string code = """
import Status.*

val s = Status.Open("foo")

match s {
    Open(val reason) => reason
    Closed(_) => ""
}

union Status {
    case Closed(reason: string)
    case Open(reason: string)
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchStatement_UserDefinedUnionCasesCanUseTargetTypedOrWildcardImportedForm()
    {
        const string code = """
import Status.*

val a = Status.Open("foo")
val b = Status.Closed("done")

val textA = match a {
    .Open(val reason) => reason
    .Closed(_) => ""
}

val textB = match b {
    Open(val reason) => reason
    Closed(_) => ""
}

union Status {
    case Closed(reason: string)
    case Open(reason: string)
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void MatchStatement_NotLastInBlock_TargetTypesArmMemberBindings_AndWarnsValueIgnored()
    {
        const string code = """
enum PingStatus {
    Ok,
    Error
}

func ping(name: string) -> PingStatus {
    match name {
        "Bob" => PingStatus.Ok
        "bob" => PingStatus.Ok
        _ => PingStatus.Error
    }

    return PingStatus.Error
}
""";

        var verifier = CreateVerifier(
            code,
            [
                new DiagnosticResult("RAV2107").WithAnySpan(),
            ]);

        verifier.Verify();
    }

    [Fact]
    public void MatchStatement_NotLastInReturningMethod_ValueArmsReportIgnoredValueWarning()
    {
        const string code = """
func evaluate(flag: bool) -> int {
    match flag {
        true => 1
        false => 0
    }

    return 42
}
""";

        var verifier = CreateVerifier(
            code,
            [
                new DiagnosticResult("RAV2107").WithAnySpan(),
            ]);

        verifier.Verify();
    }

    [Fact]
    public void MatchStatement_LastInReturningMethod_DoesNotReportIgnoredValueWarning()
    {
        const string code = """
func evaluate(flag: bool) -> int {
    match flag {
        true => 1
        false => 0
    }
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }
}
