using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public sealed class ImplementInterfaceMembersCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void MissingMembers_GeneratesMethodAndPropertyStubsInOneFix()
    {
        const string code = """
interface IService {
    func GetValue(key: string) -> int
    val Name: string { get; }
    var Count: int { get; set; }
    val self[index: int]: string { get; }
}

class Service : IService {
}
""";

        const string fixedCode = """
interface IService {
    func GetValue(key: string) -> int
    val Name: string { get; }
    var Count: int { get; set; }
    val self[index: int]: string { get; }
}

class Service : IService {
    func GetValue(key: string) -> int => throw System.NotImplementedException()

    val Name: string => throw System.NotImplementedException()

    var Count: int {
        get => throw System.NotImplementedException()
        set => throw System.NotImplementedException()
    }

    val self[index: int]: string => throw System.NotImplementedException()
}
""";

        var verifier = CreateCodeFixVerifier<NoOpAnalyzer, ImplementInterfaceMembersCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(CompilerDiagnostics.TypeDoesNotImplementAbstractMember.Id).WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void MissingGenericAndStaticMethods_PreservesSignaturesAndConstraints()
    {
        const string code = """
interface IFactory<TSelf> {
    static func Create(value: int) -> TSelf
    func Convert<T>(value: T) -> string where T: class
}

class Factory : IFactory<Factory> {
}
""";

        const string fixedCode = """
interface IFactory<TSelf> {
    static func Create(value: int) -> TSelf
    func Convert<T>(value: T) -> string where T: class
}

class Factory : IFactory<Factory> {
    static func Create(value: int) -> Factory => throw System.NotImplementedException()

    func Convert<T>(value: T) -> string where T: class => throw System.NotImplementedException()
}
""";

        var verifier = CreateCodeFixVerifier<NoOpAnalyzer, ImplementInterfaceMembersCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(CompilerDiagnostics.TypeDoesNotImplementAbstractMember.Id).WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void MissingMetadataInterfaceMethod_GeneratesStub()
    {
        const string code = """
import System.*

class Resource : IDisposable {
}
""";

        const string fixedCode = """
import System.*

class Resource : IDisposable {
    func Dispose() -> () => throw System.NotImplementedException()
}
""";

        var verifier = CreateCodeFixVerifier<NoOpAnalyzer, ImplementInterfaceMembersCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(CompilerDiagnostics.TypeDoesNotImplementAbstractMember.Id).WithAnySpan()]);

        verifier.Verify();
    }

    private sealed class NoOpAnalyzer : DiagnosticAnalyzer
    {
        public override void Initialize(AnalysisContext context)
        {
        }
    }
}
