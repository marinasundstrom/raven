using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class NativeStructuralFunctionTests
{
    private static Compilation Create(string source = "", TargetPlatform target = TargetPlatform.NeoCLR) =>
        Compilation.Create("FunctionSymbols", [SyntaxTree.ParseText(source)], TestMetadataReferences.DefaultWithRavenCore,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));

    [Theory]
    [InlineData(TargetPlatform.NeoCLR, true)]
    [InlineData(TargetPlatform.DotNet, false)]
    public void ReservedBootstrapCallbackTransportIsTargetSpecific(TargetPlatform target, bool structural)
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-callback-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        try
        {
            const string source = """
                namespace System.Runtime.CompilerServices {
                    public static class RuntimeServices {
                        public static int StartWorker(System.Func<string, string> callback) => 0;
                        public static void ScheduleTask(System.Action callback) { }
                    }
                }
                public static class Ordinary {
                    public static int Call(System.Func<string, string> callback) => 0;
                }
                """;
            var references = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net10.0"))
                .Select(path => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(path));
            var metadata = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("BootstrapCallbacks",
                [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText(source)], references,
                new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
            var path = Path.Combine(directory, "BootstrapCallbacks.dll");
            using (var output = File.Create(path))
                Assert.True(metadata.Emit(output).Success);
            var compilation = Compilation.Create("Test", [],
                TestMetadataReferences.Default.Concat([MetadataReference.CreateFromFile(path)]).ToArray(),
                new CompilationOptions().WithTargetPlatform(target));
            var service = compilation.GetTypeByMetadataName("System.Runtime.CompilerServices.RuntimeServices")!;
            var parameter = service.GetMembers("StartWorker").OfType<IMethodSymbol>().Single().Parameters[0].Type;
            Assert.Equal(structural, parameter is StructuralFunctionTypeSymbol);
            if (structural)
            {
                var action = service.GetMembers("ScheduleTask").OfType<IMethodSymbol>().Single().Parameters[0].Type;
                Assert.Equal(SpecialType.System_Void, Assert.IsType<StructuralFunctionTypeSymbol>(action).ReturnType.SpecialType);
            }
            var ordinary = compilation.GetTypeByMetadataName("Ordinary")!;
            Assert.IsNotType<StructuralFunctionTypeSymbol>(ordinary.GetMembers("Call").OfType<IMethodSymbol>().Single().Parameters[0].Type);
        }
        finally { Directory.Delete(directory, true); }
    }

    [Fact]
    public void NativeSignaturesHaveStructuralIdentityWithoutNominalDelegateAncestry()
    {
        var compilation = Create();
        var integer = compilation.GetSpecialType(SpecialType.System_Int32);
        var function = Assert.IsType<StructuralFunctionTypeSymbol>(compilation.CreateFunctionTypeSymbol([integer], integer));
        var same = compilation.CreateFunctionTypeSymbol([integer], integer);
        Assert.True(SymbolEqualityComparer.Default.Equals(function, same));
        Assert.Equal(SymbolEqualityComparer.Default.GetHashCode(function), SymbolEqualityComparer.Default.GetHashCode(same));
        Assert.False(SymbolEqualityComparer.Default.Equals(function, compilation.CreateFunctionTypeSymbol([], integer)));
        Assert.False(SymbolEqualityComparer.Default.Equals(function, compilation.GetTypeByMetadataName("System.Func`2")!.Construct(integer, integer)));
        Assert.False(function.MetadataIdentityEquals(compilation.CreateFunctionTypeSymbol([], integer)));
        Assert.False(compilation.ClassifyConversion(function, compilation.CreateFunctionTypeSymbol([], integer)).Exists);
        Assert.False(compilation.ClassifyConversion(function, compilation.GetTypeByMetadataName("System.Func`2")!.Construct(integer, integer)).Exists);
        Assert.Equal(SpecialType.System_Object, function.BaseType.SpecialType);
        Assert.Empty(function.Constructors);
        Assert.Equal("(int) -> int", function.ToDisplayString());
        Assert.True(SemanticFacts.SatisfiesFunctionTypeConstraint(function));
    }

    [Fact]
    public void NativeNoResultAndInhabitedUnitRemainDistinct()
    {
        var compilation = Create();
        var noResult = Assert.IsType<StructuralFunctionTypeSymbol>(compilation.CreateNoResultFunctionTypeSymbol([]));
        var unit = Assert.IsType<StructuralFunctionTypeSymbol>(compilation.CreateFunctionTypeSymbol([], compilation.GetSpecialType(SpecialType.System_Unit)));
        Assert.Equal(SpecialType.System_Void, noResult.ReturnType.SpecialType);
        Assert.Equal(SpecialType.System_Unit, unit.ReturnType.SpecialType);
        Assert.False(SymbolEqualityComparer.Default.Equals(noResult, unit));
    }

    [Fact]
    public void CallbackOverloadsPreserveParameterAndResultShapesDuringLookup()
    {
        var compilation = Create("""
            class Server {
                func Serve(callback: (int) -> int) -> int => callback(1)
                func Serve(callback: (string) -> int) -> int => callback("hello")
                func Serve(callback: (string) -> string) -> string => callback("hello")
            }
            func Number(value: int) -> int => value
            func Count(value: string) -> int => 42
            func Text(value: string) -> string => value
            func Test() {
                let server = Server()
                let first = server.Serve(Number)
                let second = server.Serve(Count)
                let third = server.Serve(Text)
                let typed: (string) -> int = Count
                let fourth = server.Serve(typed)
            }
            """);
        Assert.Empty(compilation.GetSemanticModel(compilation.SyntaxTrees.Single()).GetDiagnostics()
            .Where(d => d.Severity == DiagnosticSeverity.Error));
        var methods = compilation.GetTypeByMetadataName("Server")!.GetMembers("Serve").OfType<IMethodSymbol>().ToArray();
        Assert.Equal(3, methods.Select(method => method.GetLookupIdentityKey()).Distinct().Count());
        Assert.Equal(3, methods.Select(method => method.GetShallowLookupIdentityKey()).Distinct().Count());
    }

    [Fact]
    public void DotNetKeepsNominalFunctionTransport()
    {
        var compilation = Create(target: TargetPlatform.DotNet);
        var integer = compilation.GetSpecialType(SpecialType.System_Int32);
        var function = compilation.CreateFunctionTypeSymbol([integer], integer);
        Assert.True(SymbolEqualityComparer.Default.Equals(function, compilation.GetTypeByMetadataName("System.Func`2")!.Construct(integer, integer)));
        Assert.False(SemanticFacts.SatisfiesFunctionTypeConstraint(function));
    }

    [Fact]
    public void SubstitutionAndInferenceTraverseFunctionParametersAndResults()
    {
        var compilation = Create("""
            class Box<T> {
                private val callback: (T) -> T
                init(callback: (T) -> T) { self.callback = callback }
                func Get() -> (T) -> T => callback
            }
            func Apply<T>(callback: (T) -> T, value: T) -> T => callback(value)
            func Identity(value: int) -> int => value
            func Test() -> int {
                let callback: (int) -> int = Identity
                let box = Box<int>(callback)
                return Apply(box.Get(), 42)
            }
            """);
        Assert.Empty(compilation.GetSemanticModel(compilation.SyntaxTrees.Single()).GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var box = (INamedTypeSymbol)compilation.GetTypeByMetadataName("Box`1")!.Construct(compilation.GetSpecialType(SpecialType.System_Int32));
        var get = box.GetMembers("Get").OfType<IMethodSymbol>().Single();
        var result = Assert.IsType<StructuralFunctionTypeSymbol>(get.ReturnType);
        Assert.Equal(SpecialType.System_Int32, result.ReturnType.SpecialType);
        Assert.Equal(SpecialType.System_Int32, result.ParameterTypes.Single().SpecialType);
    }
}
