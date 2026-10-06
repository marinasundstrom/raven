using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public sealed class ScopeExitCleanupTests
{
    private const string ResourceSource = """
        import System.*
        import System.Threading.Tasks.*
        import System.Collections.Generic.*

        public interface ResourceProtocol {
            func Dispose() -> unit
        }
        public class Resource : ResourceProtocol {
            val Id: int
            public init(id: int) { Id = id }
            public func Dispose() -> unit { Probe.Record(Id) }
        }
        """;

    private static Compilation Create(string members, string protocol = "ResourceProtocol", bool valueResource = false)
        => Compilation.Create("UseCleanup", [SyntaxTree.ParseText((valueResource ? ResourceSource.Replace("public class Resource", "public struct Resource") : ResourceSource) + "\n" + $$"""
            public static class Probe {
                static var Log: int = 0
                public static func Record(id: int) -> int {
                    Log = Log * 10 + id
                    return 7
                }
                public static func ReadLog() -> int => Log
                {{members}}
            }
            """)], TestMetadataReferences.DefaultWithRavenCore,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithRuntimeDisposalContract(new("UseCleanup", protocol, UseExceptionHandling: false)));

    [Theory]
    [InlineData("""
        public static func Run() -> int {
            use a = Resource(1)
            use b = Resource(2)
            return Record(9)
        }
        """, 7, 921)]
    [InlineData("""
        public static func Run() -> int {
            use a = Resource(1) in {
                use b = Resource(2)
            }
            return Record(9)
        }
        """, 7, 219)]
    [InlineData("""
        public static func Run() -> int {
            use a = Resource(1)
            let value = {
                use b = Resource(2)
                Record(9)
            }
            return value
        }
        """, 7, 921)]
    [InlineData("""
        public static func Run() -> int {
            use a = Resource(1)
            var i = 0
            while i < 2 {
                use b = Resource(2)
                i = i + 1
                if i == 1 { continue }
                break
            }
            return 7
        }
        """, 7, 221)]
    [InlineData("""
        public static func Run() -> int {
            use a = Resource(1)
            for i in [1, 2] {
                use b = Resource(2)
                if i == 1 { continue }
                break
            }
            return 7
        }
        """, 7, 221)]
    [InlineData("""
        public static func Run() -> int {
            use a = Resource(1)
            let f = func () -> int {
                use b = Resource(2)
                return 7
            }
            return f()
        }
        """, 7, 21)]
    [InlineData("""
        public static func Run() -> int {
            use a = Resource(1)
            use b = Resource(2)
            Record(9)
        }
        """, 7, 921)]
    [InlineData("""
        public static func Run() -> int {
            use a = Resource(1)
            if Record(9) == 7 { return 7 }
            use b = Resource(2)
            return 0
        }
        """, 7, 91)]
    [InlineData("""
        public static func Run() -> int {
            use a = Resource(1)
            outer: for i in [1, 2] {
                use b = Resource(2)
                loop {
                    use c = Resource(3)
                    if i == 1 { continue outer }
                    break outer
                }
            }
            return 7
        }
        """, 7, 32321)]
    [InlineData("""
        public static func Run() -> int {
            use a = Resource(1)
            if true {
                use b = Resource(2)
                goto done
            }
            done: return 7
        }
        """, 7, 21)]
    [InlineData("""
        public static func Run() -> int {
            var i = 0
            again: use a = Resource(1)
            i = i + 1
            if i < 2 { goto again }
            return 7
        }
        """, 7, 11)]
    public void OrdinaryExitsDisposeActiveResourcesExactlyOnce(string members, int expectedResult, int expectedLog)
        => Execute(Create(members), expectedResult, expectedLog);

    [Fact]
    public void ValueResourcesDisposeInReverseOrder()
        => Execute(Create("""
            public static func Run() -> int {
                use a = Resource(1)
                use b = Resource(2)
                return 7
            }
            """, valueResource: true), 7, 21);

    [Theory]
    [InlineData("Result<Resource, string>", ".Error(\"failure\")", "Result<int, string>", ".Ok(7)")]
    [InlineData("Option<Resource>", ".None", "Option<int>", ".Some(7)")]
    public void FailedResourceInitializerDisposesOnlyEarlierResources(string carrier, string failure, string result, string success)
    {
        var compilation = Create($$"""
            static func Open() -> {{carrier}} => {{failure}}
            public static func Work() -> {{result}} {
                use a = Resource(1)
                use b = Open()?
                Record(9)
                return {{success}}
            }
            public static func Run() -> int {
                let ignored = Work()
                return 7
            }
            """);
        Execute(compilation, 7, 1);
    }

    [Theory]
    [InlineData("Result<int, string>", ".Error(\"failure\")", ".Ok(value)")]
    [InlineData("Option<int>", ".None", ".Some(value)")]
    public void PropagationDisposesAllActiveScopes(string carrier, string failure, string success)
    {
        var compilation = Create($$"""
            static func Read() -> {{carrier}} => {{failure}}
            public static func Work() -> {{carrier}} {
                use a = Resource(1)
                use b = Resource(2) in {
                    let value = Read()?
                    return {{success}}
                }
            }
            public static func Run() -> int {
                let ignored = Work()
                return 7
            }
            """);
        Execute(compilation, 7, 21);
    }

    [Fact]
    public void AsyncUseIsDiagnosed()
    {
        var compilation = Create("""
            public static async func Work() -> Task<int> {
                use a = Resource(1)
                await Task.Delay(1)
                return 7
            }
            """);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAVT006");
    }

    [Fact]
    public void IteratorUseIsDiagnosed()
    {
        var compilation = Create("""
            public static func Work() -> IEnumerable<int> {
                use a = Resource(1)
                yield return 7
            }
            """);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAVT006");
    }

    [Fact]
    public void MissingProtocolIsDiagnosed()
        => Assert.Contains(Create("public static func Run() { use a = Resource(1) }", "Missing").GetDiagnostics(),
            d => d.Severity == DiagnosticSeverity.Error);

    [Fact]
    public void PortableBodyPlannerAcceptsCleanupWithoutExceptionRegions()
    {
        var compilation = Create("""
            public static func Run() -> int {
                use a = Resource(1)
                return 7
            }
            """);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        var tree = compilation.SyntaxTrees.Single();
        var syntax = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single(method => method.Identifier.ValueText == "Run");
        var symbol = (IMethodSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax)!;
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsRootClassLocals: true, allowsRootClassSignatures: true, allowsInterfaceSignatures: true, allowsInterfaceDispatch: true);
        Assert.True(SourceCallablePlan.TryCreate(symbol, out var plan, capabilities));
        Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, capabilities), failure?.Detail);
    }

    [Fact]
    public void OptionsCopiesPreserveContractAndNullRestoresDefault()
    {
        var contract = new RuntimeDisposalContract("UseCleanup", "ResourceProtocol", false);
        var options = new CompilationOptions().WithRuntimeDisposalContract(contract)
            .WithOutputKind(OutputKind.DynamicallyLinkedLibrary).WithOptimizationLevel(OptimizationLevel.Release)
            .WithRunAnalyzers(false).WithRuntimeSelfTypeContract(null).WithAsyncExceptionCapture(false);
        Assert.Equal(contract, options.RuntimeDisposalContract);
        Assert.Null(options.WithRuntimeDisposalContract(null).RuntimeDisposalContract);
    }

    [Theory]
    [InlineData("Library", true)]
    [InlineData("WrongLibrary", false)]
    public void OwnershipManifestSelectsOnlyDeclaredDisposalOwner(string owner, bool valid)
    {
        var path = Path.GetTempFileName();
        try
        {
            File.WriteAllText(path, $$"""
                {
                  "Version": 1,
                  "Libraries": [{ "AssemblyName": "Library", "Sources": ["Library.rvn"],
                    "Types": ["System.Disposable", "System.Iterable`1", "System.Iterator`1"] }],
                  "Iteration": { "AssemblyName": "Library", "IterableTypeName": "System.Iterable`1", "IteratorTypeName": "System.Iterator`1" },
                  "Disposal": { "AssemblyName": "{{owner}}", "InterfaceTypeName": "System.Disposable", "UseExceptionHandling": false }
                }
                """);
            if (!valid)
            {
                Assert.Throws<InvalidDataException>(() => BootstrapOwnershipManifest.Read(path));
                return;
            }
            var options = BootstrapOwnershipManifest.Read(path).Apply(CompilationOptions.NeoCLR);
            Assert.Equal(new RuntimeDisposalContract("Library", "System.Disposable", false), options.RuntimeDisposalContract);
        }
        finally { File.Delete(path); }
    }

    [Fact]
    public void JumpPastResourceInitializationIsDiagnosed()
    {
        var compilation = Create("""
            public static func Run() -> int {
                goto done
                use a = Resource(1)
                done: return 7
            }
            """, valueResource: true);
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAVT007");
    }

    private static void Execute(Compilation compilation, int expectedResult, int expectedLog)
    {
        using var pe = new MemoryStream();
        var emitted = compilation.Emit(pe);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(pe, TestMetadataReferences.DefaultWithRavenCore);
        var probe = loaded.Assembly.GetType("Probe", throwOnError: true)!;
        Assert.Equal(expectedResult, probe.GetMethod("Run")!.Invoke(null, null));
        Assert.Equal(expectedLog, probe.GetMethod("ReadLog")!.Invoke(null, null));
        // Exception regions are a target capability, not an instruction-sequence assertion.
        foreach (var method in probe.GetMethods(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static | BindingFlags.DeclaredOnly))
            Assert.Empty(method.GetMethodBody()!.ExceptionHandlingClauses);
    }
}
