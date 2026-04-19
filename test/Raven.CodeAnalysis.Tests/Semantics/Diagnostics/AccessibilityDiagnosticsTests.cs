using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class AccessibilityDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void PrivateConstructorInvocation_ReportsRAV0500()
    {
        const string source = """
class Container {
    private init() {}
}

val instance = Container();
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult(CompilerDiagnostics.SymbolIsInaccessible.Id).WithAnySpan().WithArguments("constructor", "init() -> ()")],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PrivateFieldAccess_ReportsRAV0500()
    {
        const string source = """
class Container {
    private var secret: int;

    public init() {
        self.secret = 42;
    }
}

val instance = Container();
val value = instance.secret;
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult(CompilerDiagnostics.SymbolIsInaccessible.Id).WithAnySpan().WithArguments("property", "var secret: int")],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PublicMethodReturningInternalType_ReportsRAV0501()
    {
        const string source = """
internal union Result<T, E> {
    case Ok(value: T)
    case Error(message: E)
}

public class Container {
    public static func ParseNumber(str: string) -> Result<int, string> {
        return .Ok(0);
    }
}
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("return", "Result<int, string>", "method", "Container.ParseNumber")],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PublicMethodParameterWithInternalType_ReportsRAV0501()
    {
        const string source = """
internal class Hidden {}

public class Exposer {
    public func Call(value: Hidden) -> int {
        return 0;
    }
}
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("parameter 'value'", "Hidden", "method", "Exposer.Call")],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void ExtensionAccessingPrivatePromotedProperty_ReportsRAV0500()
    {
        const string source = """
class Foo(private var Name: string)

extension FooExtensions for Foo {
    func Peek() -> string => self.Name
}
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult(CompilerDiagnostics.SymbolIsInaccessible.Id).WithAnySpan().WithArguments("property", "var Name: string")],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void ExtensionAccessingProtectedPromotedProperty_ReportsRAV0500()
    {
        const string source = """
class Foo(protected var Name: string)

extension FooExtensions for Foo {
    func Peek() -> string => self.Name
}
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult(CompilerDiagnostics.SymbolIsInaccessible.Id).WithAnySpan().WithArguments("property", "var Name: string")],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact(Skip = "Metadata accessibility enforcement pending")]
    public void ReferencingInternalMetadataType_ReportsRAV0500()
    {
        const string librarySource = """
namespace Lib

internal class Hidden {}
""";

        var metadataReference = CreateMetadataReference(librarySource);

        const string source = """
import Lib.*

val value: Hidden = default(Hidden)
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "consumer",
            [tree],
            TestMetadataReferences.Default.Append(metadataReference).ToArray());

        var libNamespace = compilation.GlobalNamespace.LookupNamespace("Lib") as INamespaceSymbol;
        Assert.NotNull(libNamespace);

        var hiddenType = libNamespace!.GetMembers("Hidden").OfType<INamedTypeSymbol>().First();

        Assert.Equal(Accessibility.Internal, hiddenType.DeclaredAccessibility);
        Assert.False(SymbolEqualityComparer.Default.Equals(hiddenType.ContainingAssembly, compilation.Assembly));

    }

    private static MetadataReference CreateMetadataReference(string source)
    {
        return TestMetadataFactory.CreateFileReferenceFromSource(
            source,
            assemblyName: "accessibility-metadata-fixture");
    }
}
