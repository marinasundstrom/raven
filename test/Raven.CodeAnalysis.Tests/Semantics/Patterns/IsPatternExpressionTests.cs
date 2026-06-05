using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class IsPatternExpressionTests : DiagnosticTestBase
{
    [Fact]
    public void EnumMemberChecks_WithJsonValueKind_AcceptEqualityAndPatternForms()
    {
        const string code = """
import System.Text.Json.*

func Test(element: JsonElement) -> bool {
    return element.ValueKind == JsonValueKind.True ||
        element.ValueKind == .True ||
        .True == element.ValueKind ||
        element.ValueKind != .False ||
        .False != element.ValueKind ||
        element.ValueKind is JsonValueKind.True ||
        element.ValueKind is .True
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void EnumMemberChecks_WithJsonValueKind_AcceptsTargetTypedEqualityShorthand()
    {
        const string code = """
import System.Text.Json.*

func Test(element: JsonElement) -> bool {
    return element.ValueKind == .True
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void ConstantMemberCheck_WithMathPi_AcceptsQualifiedPatternForm()
    {
        const string code = """
import System.*

func Test(value: double) -> bool {
    return value is Math.PI
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }

    [Fact]
    public void ConstantMemberCheck_WithMathPi_BindsConstantPatternValue()
    {
        const string code = """
import System.*

func Test(value: double) -> bool {
    return value is Math.PI
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var isPattern = tree.GetRoot()
            .DescendantNodes()
            .OfType<IsPatternExpressionSyntax>()
            .Single();
        var bound = Assert.IsType<BoundIsPatternExpression>(model.GetBoundNode(isPattern));
        var constantPattern = Assert.IsType<BoundConstantPattern>(bound.Pattern);

        Assert.Equal(Math.PI, Assert.IsType<double>(constantPattern.ConstantValue));
    }

    [Fact]
    public void IsPattern_WithIncompatibleLiteralPattern_ReportsDiagnostic()
    {
        const string code = """
record class Foo(Value: bool, Data: (int, int))

func Test(x: object?) {
    if x is Foo(_, true) {
    }
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2102").WithAnySpan().WithArguments("bool", "(int, int)")]);

        verifier.Verify();
    }

    [Fact]
    public void IsPattern_UserDefinedUnionCasesRequireQualificationOrImport()
    {
        const string code = """
val s = Status.Open("foo")

if s is Open(val reason) {
}

union Status {
    case Closed(reason: string)
    case Open(reason: string)
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id)
                .WithAnySpan()
                .WithArguments("Open")]);

        verifier.Verify();
    }

    [Fact]
    public void IsPattern_UserDefinedUnionCasesCanBindFromWildcardImport()
    {
        const string code = """
import Status.*

val s = Status.Open("foo")

if s is Open(val reason) {
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
    public void IsPattern_UserDefinedUnionCasesCanUseTargetTypedOrWildcardImportedForm()
    {
        const string code = """
import Status.*

val a = Status.Open("foo")
val b = Status.Closed("done")

if a is .Open(val reason) {
}

if b is Closed(_) {
}

union Status {
    case Closed(reason: string)
    case Open(reason: string)
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }
}
