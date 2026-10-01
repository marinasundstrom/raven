using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class CallableDeclarationTests
{
    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void ExpressionBodiesShareLoweringAndPreserveResults(OptimizationLevel optimization)
    {
        const string source = """
            func Main() -> int => Helpers.Value(20)
            public static class Helpers {
                public static func Value(value: int) -> int => Twice(value + 1)
                private static func Twice(value: int) -> int => value * 2
                public static func Wide(value: long) -> long => value + 1L
                public static func Widen(value: int) -> long => value
                public static func Positive(value: int) -> bool => value > 0
                public static func Text(value: string) -> string => value
                public static func Finish() => Empty()
                private static func Empty() { }
            }
            """;
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("ArrowBodies", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        foreach (var syntax in tree.GetRoot().DescendantNodes().Where(n => n is MethodDeclarationSyntax or FunctionStatementSyntax))
        {
            Assert.True(SourceCallablePlan.TryCreate((IMethodSymbol)model.GetDeclaredSymbol(syntax)!, out var plan, ReflectionEmitCapabilities.Shared));
            Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, ReflectionEmitCapabilities.Shared), plan.MetadataName + ": " + failure?.Detail);
        }
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        var assembly = Assembly.Load(output.ToArray());
        Assert.Equal(42, assembly.EntryPoint!.Invoke(null, null));
        var helpers = assembly.GetType("Helpers")!;
        Assert.Equal(5000000001L, helpers.GetMethod("Wide")!.Invoke(null, [5000000000L]));
        Assert.Equal(42L, helpers.GetMethod("Widen")!.Invoke(null, [42]));
        Assert.Equal(true, helpers.GetMethod("Positive")!.Invoke(null, [1]));
        Assert.Equal("Hej 🌍", helpers.GetMethod("Text")!.Invoke(null, ["Hej 🌍"]));
        Assert.Null(helpers.GetMethod("Finish")!.Invoke(null, null));
    }

    [Fact]
    public void SharedTypeContractSeparatesNoResultFromValueTypes()
    {
        var compilation = Compilation.Create("PrimitiveContracts", [SyntaxTree.ParseText("""
            public static class Contracts {
                public static func Mix(number: int, wide: long, flag: bool) -> long { return wide }
                public static func Finish() { }
                public static func Text(value: string) -> string { return value }
                public static func Maybe(value: int?) -> int? { return value }
                public static func UnitValue(value: unit) -> unit { return value }
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(compilation.SyntaxTrees[0]);
        var methods = compilation.SyntaxTrees[0].GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>()
            .Select(d => (IMethodSymbol)model.GetDeclaredSymbol(d)!).ToDictionary(m => m.Name);
        Assert.True(PrimitiveCallableSignature.TryCreate(methods["Mix"], out var mixed));
        Assert.Equal(EmissionPrimitiveType.Int64, mixed.ReturnType);
        Assert.Equal(new[] { EmissionPrimitiveType.Int32, EmissionPrimitiveType.Int64, EmissionPrimitiveType.Boolean }, mixed.ParameterTypes);
        Assert.True(mixed.ReturnsValue);
        Assert.True(PrimitiveCallableSignature.TryCreate(methods["Finish"], out var finish));
        Assert.Equal(EmissionPrimitiveType.NoResult, finish.ReturnType);
        Assert.False(finish.ReturnsValue);
        Assert.Empty(finish.ParameterTypes);
        Assert.True(PrimitiveCallableSignature.TryCreate(methods["Text"], out var text));
        Assert.Equal(EmissionPrimitiveType.String, text.ReturnType);
        Assert.Equal(new[] { EmissionPrimitiveType.String }, text.ParameterTypes);
        foreach (var name in new[] { "Maybe", "UnitValue" })
            Assert.False(PrimitiveCallableSignature.TryCreate(methods[name], out _));
        Assert.False(EmissionPrimitiveTypes.TryGetValueType(methods["Finish"].ReturnType, out _));
    }

    [Fact]
    public void SharedDeclarationsPreserveVisibilityParametersAndGeneralGenericPath()
    {
        const string source = """
            public static class Callables {
                public static func Add(value: int, amount: int) -> int { return value + amount }
                public static func Notify(value: int) { }
                private static func Hidden() -> int { return 7 }
                internal static func Internal() -> int { Hidden() }
                public static func Identity<T>(value: T) -> T { return value }
            }
            """;
        var compilation = Compilation.Create("CallableDeclarations", [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        var type = Assembly.Load(output.ToArray()).GetType("Callables")!;
        var add = type.GetMethod("Add")!;
        Assert.True(add.IsPublic && add.IsStatic && add.IsHideBySig);
        Assert.Equal(typeof(int), add.ReturnType);
        Assert.Equal(new[] { "value", "amount" }, add.GetParameters().Select(p => p.Name));
        Assert.All(add.GetParameters(), p => Assert.Equal(typeof(int), p.ParameterType));
        Assert.Equal(42, add.Invoke(null, [40, 2]));
        var notify = type.GetMethod("Notify")!;
        Assert.Equal(typeof(void), notify.ReturnType);
        Assert.Equal("value", Assert.Single(notify.GetParameters()).Name);
        Assert.Null(notify.Invoke(null, [42]));
        var hidden = type.GetMethod("Hidden", BindingFlags.NonPublic | BindingFlags.Static)!;
        Assert.True(hidden.IsPrivate);
        Assert.Equal(7, hidden.Invoke(null, null));
        var internalMethod = type.GetMethod("Internal", BindingFlags.NonPublic | BindingFlags.Static)!;
        Assert.True(internalMethod.IsAssembly);
        Assert.Equal(7, internalMethod.Invoke(null, null));
        var generic = type.GetMethod("Identity")!;
        Assert.True(generic.IsGenericMethodDefinition);
        Assert.Equal(42, generic.MakeGenericMethod(typeof(int)).Invoke(null, [42]));
    }
    [Fact]
    public void SourcePlansSeparateAssemblyOwnershipFromCliCarriers()
    {
        const string source = """
            func Main() -> int {
                Helpers.Value()
            }
            public static class Helpers {
                public static func Value() -> int { 42 }
            }
            """;
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("SourcePlans", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(OptimizationLevel.Release));
        var model = compilation.GetSemanticModel(tree);
        var function = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single();
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        Assert.True(SourceCallablePlan.TryCreate((IMethodSymbol)model.GetDeclaredSymbol(function)!, out var functionPlan));
        Assert.True(SourceCallablePlan.TryCreate((IMethodSymbol)model.GetDeclaredSymbol(method)!, out var methodPlan));
        Assert.True(functionPlan!.IsAssemblyFunction);
        Assert.Null(functionPlan.TypeOwner);
        Assert.False(methodPlan!.IsAssemblyFunction);
        Assert.Equal("Helpers", methodPlan.TypeOwner!.Name);
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        Assert.Equal(42, Assembly.Load(output.ToArray()).EntryPoint!.Invoke(null, null));
    }
}
