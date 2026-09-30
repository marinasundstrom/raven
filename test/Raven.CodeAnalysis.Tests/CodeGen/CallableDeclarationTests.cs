using System.Reflection;

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
}
