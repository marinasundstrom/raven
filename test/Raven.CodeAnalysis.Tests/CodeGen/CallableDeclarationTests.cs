using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class CallableDeclarationTests
{
    [Fact]
    public void SharedDeclarationsPreserveVisibilityParametersAndGeneralGenericPath()
    {
        const string source = """
            public static class Callables {
                public static func Add(value: int, amount: int) -> int { return value + amount }
                public static func Notify(value: int) { }
                private static func Hidden() -> int { return 7 }
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
