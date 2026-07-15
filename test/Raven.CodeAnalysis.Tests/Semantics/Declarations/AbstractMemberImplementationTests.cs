using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class AbstractMemberImplementationTests : DiagnosticTestBase
{
    [Fact]
    public void DerivedClassWithoutAbstractMember_ReportsWarning()
    {
        const string source = """
abstract class Base {
    abstract func Bar() -> unit
}

class Derived : Base {
}
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.TypeDoesNotImplementAbstractMember.Id)
                    .WithSpan(5, 7, 5, 14)
                    .WithArguments("Derived", "Bar()", "Base")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ClassWithoutSourceInterfaceMethod_ReportsDiagnostic()
    {
        const string source = """
interface IResource {
    func Dispose() -> unit
}

class Resource : IResource {
}
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.TypeDoesNotImplementAbstractMember.Id)
                    .WithSpan(5, 7, 5, 15)
                    .WithArguments("Resource", "Dispose()", "IResource")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ClassWithoutMetadataInterfaceMethod_ReportsDiagnostic()
    {
        const string source = """
import System.*

class Resource : IDisposable {
}
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.TypeDoesNotImplementAbstractMember.Id)
                    .WithSpan(3, 7, 3, 15)
                    .WithArguments("Resource", "Dispose()", "IDisposable")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ExplicitInterfaceProperty_SatisfiesInterfaceContract()
    {
        const string source = """
interface IError {
    val Cause: IError? => null
}

class Error : IError {
    val IError.Cause: IError? => null
}
""";

        CreateVerifier(source).Verify();
    }

    [Fact]
    public void ExplicitInterfaceMethod_SatisfiesInterfaceContract()
    {
        const string source = """
interface ILogger {
    func Log(message: string) -> unit
}

class Logger : ILogger {
    func ILogger.Log(message: string) -> unit { }
}
""";

        CreateVerifier(source).Verify();
    }

    [Fact]
    public void DefaultInterfaceProperty_DoesNotRequireImplementation()
    {
        const string source = """
interface IMacroDefinition {
    val AcceptsArguments: bool => false
}

class ObservableMacro : IMacroDefinition {
}
""";

        CreateVerifier(source).Verify();
    }

    [Fact]
    public void DefaultInterfaceMethod_DoesNotRequireImplementation()
    {
        const string source = """
interface ILogger {
    func Log(message: string) -> unit { }
}

class Logger : ILogger {
}
""";

        CreateVerifier(source).Verify();
    }
}
