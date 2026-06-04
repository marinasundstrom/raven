using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class MemberCanBeStaticAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void MethodWithoutInstanceAccess_ReportsDiagnostic()
    {
        const string code = """
class MathOps {
    func Add(x: int, y: int) -> int => x + y
}
""";

        var verifier = CreateAnalyzerVerifier<MemberCanBeStaticAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(MemberCanBeStaticAnalyzer.DiagnosticId)
                    .WithSeverity(DiagnosticSeverity.Info)
                    .WithLocation(2, 10)
                    .WithArguments("Add")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void MethodUsingInstanceField_DoesNotReport()
    {
        const string code = """
class Counter {
    private field _count: int = 0

    func Increment() -> () {
        _count += 1
    }
}
""";

        var verifier = CreateAnalyzerVerifier<MemberCanBeStaticAnalyzer>(
            code,
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void MethodInvokingInstanceCallableMember_DoesNotReport()
    {
        const string code = """
class Handler {
    private val callback: () -> ()

    init(callback: () -> ()) {
        self.callback = callback
    }

    func Handle() -> () {
        callback()
    }
}
""";

        var verifier = CreateAnalyzerVerifier<MemberCanBeStaticAnalyzer>(
            code,
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void MethodUsingPromotedPrimaryConstructorProperty_DoesNotReport()
    {
        const string code = """
class Foo(var Name: string) {
    func GetName() -> string => Name
}
""";

        var verifier = CreateAnalyzerVerifier<MemberCanBeStaticAnalyzer>(
            code,
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void MethodUsingCapturedPrimaryConstructorParameter_DoesNotReport()
    {
        const string code = """
class Foo(name: string) {
    func GetName() -> string => name
}
""";

        var verifier = CreateAnalyzerVerifier<MemberCanBeStaticAnalyzer>(
            code,
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PrimaryConstructorPromotedMemberAccess_DoesNotReportEvenWhenMethodNameMatchesType()
    {
        const string code = """
class Foo(var Name: string) {
    func Foo() -> string => Name
}
""";

        var verifier = CreateAnalyzerVerifier<MemberCanBeStaticAnalyzer>(
            code,
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id, "RAV0113"]);

        verifier.Verify();
    }

    [Fact]
    public void ImplicitInterfaceImplementation_DoesNotReport()
    {
        const string code = """
interface ILogger {
    func Log(message: string) -> unit
}

class Logger : ILogger {
    func Log(message: string) -> unit { }
}
""";

        var verifier = CreateAnalyzerVerifier<MemberCanBeStaticAnalyzer>(
            code,
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void DisposableImplementation_DoesNotReport()
    {
        const string code = """
import System.*

class Disposable : IDisposable {
    func Dispose() -> unit { }
}
""";

        var verifier = CreateAnalyzerVerifier<MemberCanBeStaticAnalyzer>(
            code,
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void ExplicitInterfaceImplementation_DoesNotReport()
    {
        const string code = """
interface ILogger {
    func Log(message: string) -> unit
}

class Logger : ILogger {
    func ILogger.Log(message: string) -> unit { }
}
""";

        var verifier = CreateAnalyzerVerifier<MemberCanBeStaticAnalyzer>(
            code,
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void OverrideMethod_DoesNotReport()
    {
        const string code = """
open class Base {
    virtual func Run() -> unit { }
}

class Derived : Base {
    override func Run() -> unit { }
}
""";

        var verifier = CreateAnalyzerVerifier<MemberCanBeStaticAnalyzer>(
            code,
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
