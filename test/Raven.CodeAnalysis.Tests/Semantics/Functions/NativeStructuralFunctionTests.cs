using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class NativeStructuralFunctionTests
{
    private static Compilation Create(string source = "", TargetPlatform target = TargetPlatform.NeoCLR) =>
        Compilation.Create("FunctionSymbols", [SyntaxTree.ParseText(source)], TestMetadataReferences.DefaultWithRavenCore,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));

    [Fact]
    public void NoResultProjectionProducesInhabitedUnit()
    {
        var compilation = Create();
        var operand = compilation.CreateNoResultFunctionTypeSymbol([]);
        var unit = compilation.GetSpecialType(SpecialType.System_Unit);
        var projection = new FunctionReturnTypeSymbol(operand, unit);
        Assert.Same(unit, projection.Substitute(type => type));
    }

    [Theory]
    [InlineData("class Forwarder<T> { func Invoke: T with args { return 1 } }")]
    [InlineData("class Forwarder<T> where T: func { func Invoke: T with args { return 1 } }")]
    public void FunctionMethodsRejectUnprovenSignaturesAndArbitraryOpenResults(string source)
    {
        var compilation = Create(source);
        Assert.Contains(compilation.GetSemanticModel(compilation.SyntaxTrees.Single()).GetDiagnostics(),
            diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void FunctionMethodSyntaxIsGatedToNeoClr()
    {
        var compilation = Create("class Forwarder<T> where T: func { func Invoke: T with args { return default(ReturnType<T>) } }", TargetPlatform.DotNet);
        Assert.Contains(compilation.GetSemanticModel(compilation.SyntaxTrees.Single()).GetDiagnostics(),
            diagnostic => diagnostic.Id == "RAV2900");
    }

    [Fact]
    public void ReturnProjectionRemainsOpenAndSubstitutesWithFunctionArgument()
    {
        var compilation = Create("""
            abstract class Projector<T> where T: func {
                abstract func Read() -> ReturnType<T>
            }
            """);
        Assert.Empty(compilation.GetSemanticModel(compilation.SyntaxTrees.Single()).GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var owner = compilation.GetTypeByMetadataName("Projector`1")!;
        var method = owner.GetMembers("Read").OfType<IMethodSymbol>().Single();
        var projection = Assert.IsAssignableFrom<IFunctionReturnTypeSymbol>(method.ReturnType);
        Assert.True(SymbolEqualityComparer.Default.Equals(owner.TypeParameters[0], projection.FunctionType));
        var integer = compilation.GetSpecialType(SpecialType.System_Int32);
        var function = compilation.CreateFunctionTypeSymbol([], integer);
        var closed = (INamedTypeSymbol)owner.Construct(function);
        Assert.True(SymbolEqualityComparer.Default.Equals(integer,
            closed.GetMembers("Read").OfType<IMethodSymbol>().Single().ReturnType));
    }

    [Fact]
    public void FunctionMethodBindsNullableObjectArgumentsAndMaterializesParameters()
    {
        const string source = """
            class Forwarder<T> where T: func {
                func Invoke: T with args {
                    return default(ReturnType<T>)
                }
            }
            """;
        var compilation = Create(source);
        var tree = compilation.SyntaxTrees.Single();
        Assert.Equal(source, tree.GetRoot().ToFullString());
        Assert.Empty(tree.GetDiagnostics());
        var model = compilation.GetSemanticModel(tree);
        Assert.Empty(model.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var syntax = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        Assert.Equal("args", syntax.ArgumentsBinding!.Identifier.ValueText);
        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(syntax));
        Assert.NotNull(method.DeclaredFunctionType);
        var array = Assert.IsAssignableFrom<IArrayTypeSymbol>(method.Parameters.Single().Type);
        Assert.True(array.ElementType.IsNullable);
        var owner = compilation.GetTypeByMetadataName("Forwarder`1")!;
        var integer = compilation.GetSpecialType(SpecialType.System_Int32);
        var shape = compilation.CreateFunctionTypeSymbol([integer, integer], integer);
        var closed = ((INamedTypeSymbol)owner.Construct(shape)).GetMembers("Invoke").OfType<IMethodSymbol>().Single();
        Assert.Equal(2, closed.Parameters.Length);
        Assert.True(SymbolEqualityComparer.Default.Equals(integer, closed.ReturnType));
    }

    [Theory]
    [InlineData(TargetPlatform.DotNet, false)]
    [InlineData(TargetPlatform.DotNet, true)]
    [InlineData(TargetPlatform.NeoCLR, false)]
    [InlineData(TargetPlatform.NeoCLR, true)]
    public void ExplicitGenericExtensionArgumentsContextualizeFactoryResults(TargetPlatform target, bool block)
    {
        var expression = "Choice<string>.Some(\"text\")";
        var body = block ? "{ return " + expression + " }" : expression;
        var compilation = Create($$"""
            class Services { }
            union Choice<T> { case Some(value: T) }
            extension Registration for Services {
                func Add<T>(factory: (int) -> Choice<T>) -> Choice<T> => factory(0)
            }
            func Test() {
                let services = Services()
                let result = services.Add<string>(number => {{body}})
            }
            """, target);
        Assert.Empty(compilation.GetSemanticModel(compilation.SyntaxTrees.Single()).GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
    }

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
