using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class PortableDefaultReceiverTests
{
    [Theory]
    [Trait("Target", "DotNet")]
    [InlineData("private val counter: Counter = default(Counter)", "counter", 2)]
    [InlineData("private val counter: Counter = default(Counter)", "self.counter", 2)]
    [InlineData("private var counter: Counter = default(Counter)", "counter", 3)]
    [InlineData("private static val counter: Counter = default(Counter)", "counter", 2)]
    public void FieldReceiverPreservesReadonlyStorage(string declaration, string receiver, int expected)
    {
        var tree = SyntaxTree.ParseText($$"""
            public struct Counter {
                public field Value: int
                public func Bump() -> int { Value += 1; return Value }
            }
            public class Holder {
                {{declaration}}
                public func Read() -> int => {{receiver}}.Bump() + {{receiver}}.Bump()
            }
            """);
        var compilation = Compilation.Create("ReadOnlyReceiver", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single(m => m.Identifier.ValueText == "Read");
        var symbol = (IMethodSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax)!;
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsRootClassLocals: true, allowsRootClassSignatures: true, allowsManagedReferences: true, allowsStaticFields: true);
        Assert.True(SourceCallablePlan.TryCreate(symbol, out var plan, capabilities));
        Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, capabilities), failure?.Detail);
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, TestMetadataReferences.Default);
        var type = loaded.Assembly.GetType("Holder")!;
        var holder = Activator.CreateInstance(type);
        Assert.Equal(expected, type.GetMethod("Read")!.Invoke(holder, null));
        Assert.Equal(expected == 2 ? 2 : 7, type.GetMethod("Read")!.Invoke(holder, null));
    }

    [Fact]
    [Trait("Target", "DotNet")]
    public void ConstructorCanMutateItsReadonlyFieldBeforeSubsequentCallsUseCopies()
    {
        var tree = SyntaxTree.ParseText("""
            public struct Counter {
                public field Value: int
                public func Bump() -> int { Value += 1; return Value }
            }
            public class Holder {
                private val counter: Counter = default(Counter)
                public init() { counter.Bump() }
                public func Read() -> int => counter.Bump() + counter.Bump()
            }
            """);
        var compilation = Compilation.Create("ReadonlyConstructor", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, TestMetadataReferences.Default);
        var type = loaded.Assembly.GetType("Holder")!;
        var holder = Activator.CreateInstance(type);
        Assert.Equal(4, type.GetMethod("Read")!.Invoke(holder, null));
        Assert.Equal(4, type.GetMethod("Read")!.Invoke(holder, null));
    }

    [Fact]
    public void DefaultValueReceiverUsesTemporaryStorage()
    {
        var tree = SyntaxTree.ParseText("""
            public struct Counter {
                public field Value: int
                public func Bump() -> int {
                    Value = Value + 1
                    return Value
                }
            }
            public static class Consumer {
                public static func Run() -> int => default(Counter).Bump()
            }
            """);
        var compilation = Compilation.Create("DefaultReceiver", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single(m => m.Identifier.ValueText == "Run");
        var symbol = (IMethodSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax)!;
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsRootClassLocals: true, allowsRootClassSignatures: true, allowsManagedReferences: true);
        Assert.True(SourceCallablePlan.TryCreate(symbol, out var plan, capabilities));
        Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, capabilities), failure?.Detail);
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, TestMetadataReferences.Default);
        Assert.Equal(1, loaded.Assembly.GetType("Consumer")!.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!.Invoke(null, null));
    }
}
