using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public sealed class PortableEnumerationCleanupTests
{
    [Theory]
    [InlineData("for x in Items() { Probe.Record(x) }", 442)]
    [InlineData("for x in Items() { break }", 2)]
    [InlineData("for x in Items() { continue }", 2)]
    [InlineData("for x in Items() { return 7 }", 2)]
    [InlineData("use outer = Resource(1)\nfor x in Items() { use inner = Resource(3)\nreturn 7 }", 321)]
    [InlineData("outer: for x in Items() { for y in Items() { break outer } }", 22)]
    [InlineData("outer: for x in Items() { for y in Items() { continue outer } }", 222)]
    [InlineData("for x in Items() { goto done }\ndone: return 7", 2)]
    [InlineData("let missing: Items? = null\nlet items = missing ?? return 7\nfor x in items { break }", 0)]
    [InlineData("for x in Items() { match x { _ => return 7 } }", 2)]
    public void ScopedProtocolDisposesInLifetimeOrder(string body, int expectedLog)
    {
        var source = $$"""
            public interface ResourceProtocol { func Dispose() -> unit }
            public interface Iterable<T> { func GetIterator() -> Cursor<T> }
            public interface Cursor<T> : ResourceProtocol {
                func MoveNext() -> bool
                val Current: T { get }
            }
            public class Resource : ResourceProtocol {
                val Id: int
                init(id: int) { Id = id }
                func Dispose() -> unit { Probe.Record(Id) }
            }
            public class Items : Iterable<int> {
                func GetIterator() -> Cursor<int> => ItemCursor()
            }
            public class ItemCursor : Cursor<int> {
                private var index = 0
                func MoveNext() -> bool { index = index + 1; return index <= 2 }
                val Current: int => 4
                func Dispose() -> unit { Probe.Record(2) }
            }
            public static class Probe {
                static var Log: int = 0
                public static func Record(value: int) { Log = Log * 10 + value }
                public static func ReadLog() -> int => Log
                public static func Run() -> int {
                    {{body}}
                    return 7
                }
            }
            """;
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithRuntimeIterationContract(new("ForCleanup", "Iterable`1", "Cursor`1"))
            .WithRuntimeDisposalContract(new("ForCleanup", "ResourceProtocol", UseExceptionHandling: false));
        var compilation = Compilation.Create("ForCleanup", [SyntaxTree.ParseText(source)],
            TestMetadataReferences.DefaultWithRavenCore, options);
        var tree = compilation.SyntaxTrees.Single();
        var syntax = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single(method => method.Identifier.ValueText == "Run");
        var symbol = (IMethodSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax)!;
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsRootClassLocals: true, allowsRootClassSignatures: true, allowsInterfaceSignatures: true, allowsInterfaceDispatch: true,
            allowsGenericInstanceMethods: true, allowsGenericClassOwners: true, allowsReferenceEnumeration: true, allowsGenericInterfaceDeclarations: true,
            allowsConstructedInterfaceInheritance: true, allowsConstructedInterfaceImplementations: true);
        Assert.True(SourceCallablePlan.TryCreate(symbol, out var plan, capabilities));
        Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, capabilities), failure?.Detail);
        using var pe = new MemoryStream();
        var result = compilation.Emit(pe);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        var probe = Assembly.Load(pe.ToArray()).GetType("Probe")!;
        Assert.Equal(7, probe.GetMethod("Run")!.Invoke(null, null));
        Assert.Equal(expectedLog, probe.GetMethod("ReadLog")!.Invoke(null, null));
    }
}
