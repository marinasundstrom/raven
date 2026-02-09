using System;
using System.IO;
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

let instance = Container();
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0500").WithSpan(5, 16, 5, 25).WithArguments("constructor", "init() → ()")],
            disabledDiagnostics: ["RAV1014"]);

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

let instance = Container();
let value = instance.secret;
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0500").WithSpan(10, 22, 10, 28).WithArguments("field", "secret")],
            disabledDiagnostics: ["RAV1014"]);

        verifier.Verify();
    }

    [Fact]
    public void PublicMethodReturningInternalType_ReportsRAV0501()
    {
        const string source = """
public class Container {
    public static ParseNumber(str: string) -> Result<int, string> {
        return .Ok(0);
    }
}

union Result<T> {
    Ok(value: T)
    Error(message: string)
}
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0501").WithSpan(2, 47, 2, 58).WithArguments("return", "Result<int, string>", "method", "Container.ParseNumber")],
            disabledDiagnostics: ["RAV1014"]);

        verifier.Verify();
    }

    [Fact]
    public void PublicMethodParameterWithInternalType_ReportsRAV0501()
    {
        const string source = """
internal class Hidden {}

public class Exposer {
    public Call(value: Hidden) -> int {
        return 0;
    }
}
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0501").WithSpan(4, 24, 4, 30).WithArguments("parameter 'value'", "Hidden", "method", "Exposer.Call")],
            disabledDiagnostics: ["RAV1014"]);

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

let value: Hidden = default(Hidden)
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
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "lib",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        var assemblyPath = Path.Combine(Path.GetTempPath(), $"raven-metadata-{Guid.NewGuid():N}.dll");
        File.WriteAllBytes(assemblyPath, peStream.ToArray());

        return MetadataReference.CreateFromFile(assemblyPath);
    }
}
