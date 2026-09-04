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
            [new DiagnosticResult(CompilerDiagnostics.SymbolIsInaccessible.Id).WithAnySpan().WithArguments("constructor", "init() -> ()")],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PrivateFieldAccess_ReportsRAV0500()
    {
        const string source = """
class Container {
    private var secret: int { get; set; }

    public init() {
        self.secret = 42;
    }
}

let instance = Container();
let value = instance.secret;
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
    public void PublicMethodReturningInternalGenericWithInternalTypeArgument_ReportsBothTypes()
    {
        const string source = """
internal class Wrapper<T> {}
internal class Hidden {}

public class Exposer {
    public func GetValue() -> Wrapper<Hidden> {
        return Wrapper<Hidden>()
    }
}
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("return", "Wrapper<Hidden>", "method", "Exposer.GetValue"),
                new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("return", "Hidden", "method", "Exposer.GetValue"),
            ],
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
    public void PublicMethodConstructedParameterWithInternalTypeArgument_ReportsSpecificTypeArgument()
    {
        const string source = """
public class Wrapper<T> {}
internal class Hidden {}

public class Exposer {
    public func Call(value: Wrapper<Hidden>) {}
}
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("parameter 'value'", "Hidden", "method", "Exposer.Call")],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PublicPrimaryConstructorAndPromotedPropertyWithInternalType_ReportRAV0501()
    {
        const string source = """
public class Item(
    val Id: ItemId,
    var Name: string)

record ItemId private (Value: int)
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("parameter 'Id'", "ItemId", "constructor", "Item..ctor"),
                new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("property", "ItemId", "property", "Item.Id"),
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PublicDelegateWithInternalSignatureTypes_ReportsRAV0501()
    {
        const string source = """
internal class Hidden {}

public delegate Factory(value: Hidden) -> Hidden
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("return", "Hidden", "delegate", "Factory"),
                new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("parameter 'value'", "Hidden", "delegate", "Factory"),
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PublicDelegateNestedInInternalType_CanUseInternalSignatureTypes()
    {
        const string source = """
internal class Hidden {}

internal class Container {
    public delegate Factory(value: Hidden) -> Hidden
}
""";

        var verifier = CreateVerifier(
            source,
            [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PublicDelegateWithInternalConstraint_ReportsRAV0501()
    {
        const string source = """
internal interface Hidden {}

public delegate Factory<T>() where T: Hidden
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("constraint", "Hidden", "delegate", "Factory<T>")],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void ProtectedMethodWithInternalParameterType_ReportsRAV0501()
    {
        const string source = """
internal class Hidden {}

public class Exposer {
    protected func Call(value: Hidden) {}
}
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("parameter 'value'", "Hidden", "method", "Exposer.Call")],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PublicTypeWithInternalBaseType_ReportsRAV0501()
    {
        const string source = """
internal open class Hidden {}
public class Visible : Hidden {}
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("base", "Hidden", "type", "Visible")],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PublicInterfaceWithInternalBaseInterface_ReportsRAV0501()
    {
        const string source = """
internal interface Hidden {}
public interface Visible : Hidden {}
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("base interface", "Hidden", "type", "Visible")],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PublicMemberWithInternalGenericTypeArgument_ReportsRAV0501()
    {
        const string source = """
public class Wrapper<T> {}
internal class Hidden {}

public class Exposer {
    public val Value: Wrapper<Hidden>
}
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("property", "Hidden", "property", "Exposer.Value")],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PublicMethodReturningPublicGenericWithInternalTypeArgument_ReportsSpecificTypeArgument()
    {
        const string source = """
public union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}

public record ItemId private (Value: int) {
    static func Create(value: int) -> Result<ItemId, ItemIdError> {
        return .Error(.OutOfRange)
    }
}

union ItemIdError {
    case OutOfRange
}
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("return", "ItemIdError", "method", "ItemId.Create")],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PublicMemberUsingMultipleInternalGenericTypeArguments_ReportsEachSpecificTypeArgument()
    {
        const string source = """
public class Pair<TFirst, TSecond> {}
internal class FirstHidden {}
internal class SecondHidden {}

public class Exposer {
    public val Value: Pair<FirstHidden, SecondHidden>
}
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("property", "FirstHidden", "property", "Exposer.Value"),
                new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("property", "SecondHidden", "property", "Exposer.Value"),
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PublicEventAndIndexerReportInaccessibleConstructedComponents()
    {
        const string source = """
public class Wrapper<T> {}
public delegate Handler<T>()
internal class HiddenEvent {}
internal class HiddenKey {}
internal class HiddenValue {}

public class Exposer {
    public event Changed: Handler<HiddenEvent>?

    public val self[key: HiddenKey]: Wrapper<HiddenValue> {
        get => Wrapper<HiddenValue>()
    }
}
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("event", "HiddenEvent", "event", "Exposer.Changed"),
                new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("indexer", "HiddenValue", "indexer", "Exposer.Item"),
                new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("parameter 'key'", "HiddenKey", "indexer", "Exposer.Item"),
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PublicTypeWithConstructedInternalConstraint_ReportsSpecificTypeArgument()
    {
        const string source = """
public interface Constraint<T> {}
internal class Hidden {}

public class Container<T> where T: Constraint<Hidden> {}
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("constraint", "Hidden", "type", "Container<T>")],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PublicInterfaceAndUnionWithInternalConstraints_ReportRAV0501()
    {
        const string source = """
internal interface Hidden {}

public interface Contract<T> where T: Hidden {}

public union Outcome<T> where T: Hidden {
    case Value(value: T)
}
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("constraint", "Hidden", "type", "Contract<T>"),
                new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("constraint", "Hidden", "union", "Outcome<T>"),
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PublicMethodAndNamespaceFunctionWithInternalConstraints_ReportRAV0501()
    {
        const string source = """
internal interface Hidden {}

public class Exposer {
    public func Method<T>() where T: Hidden {}
}

public func Function<T>() where T: Hidden {}
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("constraint", "Hidden", "method", "Exposer.Method"),
                new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("constraint", "Hidden", "function", "Function"),
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PublicMethodInInternalType_CanUseInternalConstraint()
    {
        const string source = """
internal interface Hidden {}

internal class Exposer {
    public func Method<T>() where T: Hidden {}
}
""";

        var verifier = CreateVerifier(
            source,
            [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PublicMacroWithInternalConstraint_ReportsRAV0501()
    {
        const string source = """
internal interface Hidden {}

public macro Generate<T>() where T: Hidden {}
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("constraint", "Hidden", "macro", "Generate")],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PublicMacroWithInternalParameterType_ReportsRAV0501()
    {
        const string source = """
internal class Hidden {}

public macro Generate(value: Hidden) {}
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("parameter 'value'", "Hidden", "macro", "Generate")],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PublicNamespaceFunctionWithInternalSignatureTypes_ReportsRAV0501()
    {
        const string source = """
internal class Hidden {}

public func Transform(value: Hidden) -> Hidden => value
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("return", "Hidden", "function", "Transform"),
                new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("parameter 'value'", "Hidden", "function", "Transform"),
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void PublicUnionCaseWithInternalPayloadType_ReportsRAV0501()
    {
        const string source = """
internal class Hidden {}

public union Outcome {
    case Success(value: int)
    case Failure(error: Hidden)
}
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0501").WithAnySpan().WithArguments("parameter 'error'", "Hidden", "union case", "Outcome.Failure")],
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

    [Fact]
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

        var diagnostics = compilation.GetDiagnostics()
            .Where(diagnostic => diagnostic.Id == CompilerDiagnostics.SymbolIsInaccessible.Id)
            .ToArray();

        Assert.Equal(2, diagnostics.Length);
        Assert.All(diagnostics, diagnostic => Assert.Equal(["type", "Hidden"], diagnostic.GetMessageArgs()));
    }

    [Fact]
    public void NamespaceLevelTypeWithoutModifier_IsInternalAcrossAssemblies()
    {
        const string librarySource = """
namespace Lib

class Hidden {}
public class Exported {}
""";

        var metadataReference = CreateMetadataReference(librarySource);

        const string source = """
import Lib.*

let hidden: Hidden = default(Hidden)
let exported: Exported = Exported()
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "consumer",
            [tree],
            TestMetadataReferences.Default.Append(metadataReference).ToArray());

        var diagnostics = compilation.GetDiagnostics()
            .Where(diagnostic => diagnostic.Id == CompilerDiagnostics.SymbolIsInaccessible.Id)
            .ToArray();

        Assert.Equal(2, diagnostics.Length);
        Assert.All(diagnostics, diagnostic => Assert.Equal(["type", "Hidden"], diagnostic.GetMessageArgs()));
        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.GetMessage().Contains("Exported"));
    }

    private static MetadataReference CreateMetadataReference(string source)
    {
        return TestMetadataFactory.CreateFileReferenceFromSource(
            source,
            assemblyName: "accessibility-metadata-fixture");
    }
}
