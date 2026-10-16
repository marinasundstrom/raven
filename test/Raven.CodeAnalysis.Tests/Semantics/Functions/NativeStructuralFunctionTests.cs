using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class NativeStructuralFunctionTests
{
    private static Compilation Create(string source = "", TargetPlatform target = TargetPlatform.NeoCLR) =>
        Compilation.Create("FunctionSymbols", [SyntaxTree.ParseText(source)], TestMetadataReferences.DefaultWithRavenCore,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));

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
