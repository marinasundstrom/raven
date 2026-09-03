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

let state: State = default

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

let state: State = default

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

let state: State = .On

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

let state: State = default

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
    public void MatchStatement_WithQualifiedSingleCaseUnionPayload_IsExhaustive()
    {
        const string librarySource = """
public union Container<T, E> {
    case Value(value: T)
    case Problem(error: E)
}
""";
        var issueTree = SyntaxTree.ParseText("""
union Issue {
    case Only
}
""");

        var consumerTree = SyntaxTree.ParseText("""
func produce() -> Container<int, Issue> => .Value(1)

func inspect() {
    let result = produce()

    match result {
        .Value(let _) => ()
        .Problem(Issue.Only) => ()
    }
}
""");
        var compilation = Compilation.Create(
            "match_statement_qualified_single_case_union_payload_from_metadata",
            [issueTree, consumerTree],
            [.. TestMetadataReferences.Default,
                TestMetadataFactory.CreateFromSource(librarySource, "match_statement_outer_union_library")],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        compilation.EnsureSetup();
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);

        var match = consumerTree.GetRoot().DescendantNodes().OfType<MatchStatementSyntax>().Single();
        var info = compilation.GetSemanticModel(consumerTree).GetMatchExhaustiveness(match);

        Assert.True(info.IsExhaustive, $"Expected exhaustive match but missing: [{string.Join(", ", info.MissingCases)}]");
        Assert.Empty(info.MissingCases);
    }

    [Theory]
    [InlineData(false, false, false)]
    [InlineData(false, false, true)]
    [InlineData(false, true, false)]
    [InlineData(false, true, true)]
    [InlineData(true, false, false)]
    [InlineData(true, false, true)]
    [InlineData(true, true, false)]
    [InlineData(true, true, true)]
    public void MatchStatement_WithNestedMetadataUnionPayload_TracksAllCases(
        bool qualifyCases,
        bool omitSecondCase,
        bool diagnosticsFirst)
    {
        const string librarySource = """
public union Container<T, E> {
    case Value(value: T)
    case Problem(error: E)
}
""";
        var issueTree = SyntaxTree.ParseText("""
union Issue {
    case First
    case Second
}
""");
        var firstPattern = qualifyCases ? "Issue.First" : ".First";
        var secondArm = omitSecondCase
            ? string.Empty
            : $"        .Problem({(qualifyCases ? "Issue.Second" : ".Second")}) => ()";
        var consumerTree = SyntaxTree.ParseText($$"""
func produce() -> Container<int, Issue> => .Value(1)

func inspect() {
    let result = produce()

    match result {
        .Value(let _) => ()
        .Problem({{firstPattern}}) => ()
{{secondArm}}
    }
}
""");
        var compilation = Compilation.Create(
            "match_statement_nested_metadata_union_payload_coverage",
            [issueTree, consumerTree],
            [.. TestMetadataReferences.Default,
                TestMetadataFactory.CreateFromSource(librarySource, "match_statement_outer_union_library")],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        compilation.EnsureSetup();
        var match = consumerTree.GetRoot().DescendantNodes().OfType<MatchStatementSyntax>().Single();
        var model = compilation.GetSemanticModel(consumerTree);
        MatchExhaustivenessInfo info;
        Diagnostic[] diagnostics;
        if (diagnosticsFirst)
        {
            diagnostics = compilation.GetDiagnostics().ToArray();
            info = model.GetMatchExhaustiveness(match);
        }
        else
        {
            info = model.GetMatchExhaustiveness(match);
            diagnostics = compilation.GetDiagnostics().ToArray();
        }

        var exhaustivenessDiagnostics = diagnostics
            .Where(diagnostic => diagnostic.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive)
            .ToArray();
        Assert.Empty(diagnostics.Except(exhaustivenessDiagnostics));

        if (omitSecondCase)
        {
            var diagnostic = Assert.Single(exhaustivenessDiagnostics);
            Assert.Contains("Problem(.Second)", diagnostic.GetMessage(), StringComparison.Ordinal);
            Assert.False(info.IsExhaustive);
            Assert.Collection(info.MissingCases, missing => Assert.Equal("Problem(.Second)", missing));
        }
        else
        {
            Assert.Empty(exhaustivenessDiagnostics);
            Assert.True(info.IsExhaustive);
            Assert.Empty(info.MissingCases);
        }
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
let result: Result<int> = .Ok(value: 1)

match result {
    .Ok(let payload) => payload
    .Error(let message) => 0
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
let s = Status.Open("foo")

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

let s = Status.Open("foo")

match s {
    Open(let reason) => reason
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

let a = Status.Open("foo")
let b = Status.Closed("done")

let textA = match a {
    .Open(let reason) => reason
    .Closed(_) => ""
}

let textB = match b {
    Open(let reason) => reason
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
