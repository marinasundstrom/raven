using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public sealed class DisposableObjectAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void DisposableReturnAssignedToLocal_ReportsDiagnosticWhenNotDisposed()
    {
        const string code = """
import System.*

class Resource : IDisposable {
    public init() {}
    public func Dispose() -> unit {}
}

func CreateResource() -> Resource {
    Resource()
}

func Test() -> () {
    val resource = CreateResource()
}
""";

        var verifier = CreateAnalyzerVerifier<DisposableObjectAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(DisposableObjectAnalyzer.DiagnosticId)
                    .WithSpan(13, 9, 13, 17)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id, UnusedVariableAnalyzer.DiagnosticId]);

        verifier.Verify();
    }

    [Fact]
    public void DisposableReturnAssignedToUseDeclaration_DoesNotReport()
    {
        const string code = """
import System.*

class Resource : IDisposable {
    public init() {}
    public func Dispose() -> unit {}
}

func CreateResource() -> Resource {
    Resource()
}

func Test() -> () {
    use resource = CreateResource()
}
""";

        var verifier = CreateAnalyzerVerifier<DisposableObjectAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id, UnusedVariableAnalyzer.DiagnosticId]);

        verifier.Verify();
    }

    [Fact]
    public void DisposableReturnAssignedToLocal_DoesNotReportWhenDisposeCalled()
    {
        const string code = """
import System.*

class Resource : IDisposable {
    public init() {}
    public func Dispose() -> unit {}
}

func CreateResource() -> Resource {
    Resource()
}

func Test() -> () {
    val resource = CreateResource()
    resource.Dispose()
}
""";

        var verifier = CreateAnalyzerVerifier<DisposableObjectAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void DisposableReturnAsBareExpression_ReportsDiagnostic()
    {
        const string code = """
import System.*

class Resource : IDisposable {
    public init() {}
    public func Dispose() -> unit {}
}

func CreateResource() -> Resource {
    Resource()
}

func Test() -> () {
    CreateResource()
}
""";

        var verifier = CreateAnalyzerVerifier<DisposableObjectAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(DisposableObjectAnalyzer.DiagnosticId)
                    .WithSpan(13, 5, 13, 21)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void DisposableReturnDisposedImmediately_DoesNotReport()
    {
        const string code = """
import System.*

class Resource : IDisposable {
    public init() {}
    public func Dispose() -> unit {}
}

func CreateResource() -> Resource {
    Resource()
}

func Test() -> () {
    CreateResource().Dispose()
}
""";

        var verifier = CreateAnalyzerVerifier<DisposableObjectAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void DisposableReturnAssignedToDiscard_ReportsDiagnostic()
    {
        const string code = """
import System.*

class Resource : IDisposable {
    public init() {}
    public func Dispose() -> unit {}
}

func CreateResource() -> Resource {
    Resource()
}

func Test() -> () {
    _ = CreateResource()
}
""";

        var verifier = CreateAnalyzerVerifier<DisposableObjectAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(DisposableObjectAnalyzer.DiagnosticId)
                    .WithSpan(13, 9, 13, 25)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void DisposableObjectCreationAssignedToLocal_ReportsDiagnosticWhenNotDisposed()
    {
        const string code = """
import System.*

class Resource : IDisposable {
    public init() {}
    public func Dispose() -> unit {}
}

func Test() -> () {
    val resource = Resource()
}
""";

        var verifier = CreateAnalyzerVerifier<DisposableObjectAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(DisposableObjectAnalyzer.DiagnosticId)
                    .WithSpan(9, 9, 9, 17)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id, UnusedVariableAnalyzer.DiagnosticId]);

        verifier.Verify();
    }

    [Fact]
    public void DisposableObjectCreationAsBareExpression_ReportsConstructorDiagnostic()
    {
        const string code = """
import System.*

class Resource : IDisposable {
    public init() {}
    public func Dispose() -> unit {}
}

func Test() -> () {
    Resource()
}
""";

        var verifier = CreateAnalyzerVerifier<DisposableObjectAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(DisposableObjectAnalyzer.DiagnosticId)
                    .WithSpan(9, 5, 9, 15)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
