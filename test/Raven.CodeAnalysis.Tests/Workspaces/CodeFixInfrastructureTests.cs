using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public class CodeFixInfrastructureTests : CodeFixTestBase
{
    [Fact]
    public void ApplyCodeFixes_MissingReturnTypeAnnotation_AddsArrowTypeClause()
    {
        var code = """
func Test() {
    return 1
}
""";

        var fixedCode = """
func Test() -> int {
    return 1
}
""";

        var verifier = CreateCodeFixVerifier<MissingReturnTypeAnnotationAnalyzer, MissingReturnTypeAnnotationCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(MissingReturnTypeAnnotationAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void ApplyCodeFixes_PreferTargetTypedUnionCase_RewritesDeclaration()
    {
        var code = """
func Test() {
    val v = Option<int>.Some(0)
}

union Option<T> {
    Some(value: T)
    None
}
""";

        var fixedCode = """
func Test() {
    val v: Option<int> = .Some(0)
}

union Option<T> {
    Some(value: T)
    None
}
""";

        var verifier = CreateCodeFixVerifier<PreferTargetTypedUnionCaseAnalyzer, PreferTargetTypedUnionCaseCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(PreferTargetTypedUnionCaseAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void ApplyCodeFixes_PreferTargetTypedUnionCaseInTargetTypedContext_RewritesInvocationArgument()
    {
        var code = """
func Foo(x: Shape) {}

func Test() {
    Foo(Shape.Circle(2))
}

union Shape {
    Circle(radius: int)
    Rectangle(width: int, height: int)
}
""";

        var fixedCode = """
func Foo(x: Shape) {}

func Test() {
    Foo(.Circle(2))
}

union Shape {
    Circle(radius: int)
    Rectangle(width: int, height: int)
}
""";

        var verifier = CreateCodeFixVerifier<PreferTargetTypedUnionCaseInTargetTypedContextAnalyzer, PreferTargetTypedUnionCaseInTargetTypedContextCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(PreferTargetTypedUnionCaseInTargetTypedContextAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void ApplyCodeFixes_NonNullDeclarations_RewritesNullableTypeToOption()
    {
        var code = """
func Test() {
    var value: int? = null
}
""";

        var fixedCode = """
func Test() {
    var value: Option<int> = null
}
""";

        var verifier = CreateCodeFixVerifier<NonNullDeclarationsAnalyzer, NonNullDeclarationsCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(NonNullDeclarationsAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void ApplyCodeFixes_PreferDuLinqExtensions_RewritesMethodName()
    {
        var code = """
import System.Linq.*

func Test() {
    val arr = [1, 2, 3]
    val first = arr.First()
}
""";

        var fixedCode = """
import System.Linq.*

func Test() {
    val arr = [1, 2, 3]
    val first = arr.FirstOrError(() => "TODO: provide error")
}
""";

        var verifier = CreateCodeFixVerifier<PreferDuLinqExtensionsAnalyzer, PreferDuLinqExtensionsCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(PreferDuLinqExtensionsAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void ApplyCodeFixes_PreferDuLinqExtensions_FirstOrDefault_RewritesToFirstOrNone()
    {
        var code = """
import System.Linq.*

func Test() {
    val arr = [1, 2, 3]
    val first = arr.FirstOrDefault()
}
""";

        var fixedCode = """
import System.Linq.*

func Test() {
    val arr = [1, 2, 3]
    val first = arr.FirstOrNone()
}
""";

        var verifier = CreateCodeFixVerifier<PreferDuLinqExtensionsAnalyzer, PreferDuLinqExtensionsCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(PreferDuLinqExtensionsAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void ApplyCodeFixes_PreferIsNullOverEquality_RewritesToIsNotNull()
    {
        var code = """
class C
{
    Run(x: int?) -> unit
    {
        if x != null { }
    }
}
""";

        var fixedCode = """
class C
{
    Run(x: int?) -> unit
    {
        if x is not null { }
    }
}
""";

        var verifier = CreateCodeFixVerifier<PreferIsNullOverEqualityAnalyzer, PreferIsNullOverEqualityCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(PreferIsNullOverEqualityAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }
}
