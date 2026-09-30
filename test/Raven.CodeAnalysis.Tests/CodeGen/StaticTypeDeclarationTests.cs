using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class StaticTypeDeclarationTests
{
    [Fact]
    public void SharedStaticTypesPreserveNamespaceIdentityAndClassFlags()
    {
        const string source = """
            namespace Alpha {
                public static class Tools {
                    public static func Value() -> int { return Beta.Tools.Value() + 2 }
                }
            }
            namespace Beta {
                public static class Tools {
                    public static func Value() -> int { return 40 }
                }
            }
            """;
        var assembly = Emit(source);
        var alpha = assembly.GetType("Alpha.Tools")!;
        var beta = assembly.GetType("Beta.Tools")!;
        foreach (var type in new[] { alpha, beta })
        {
            Assert.True(type.IsClass && type.IsPublic && type.IsAbstract && type.IsSealed);
            Assert.False(type.IsGenericType);
            Assert.Null(type.DeclaringType);
            Assert.Equal(typeof(object), type.BaseType);
        }
        Assert.Equal(42, alpha.GetMethod("Value")!.Invoke(null, null));
        Assert.Equal(40, beta.GetMethod("Value")!.Invoke(null, null));
    }

    [Fact]
    public void GenericNestedAndInstanceTypesRetainGeneralConstruction()
    {
        const string source = """
            public static class Generic<T> {
                public static func Identity(value: T) -> T { return value }
            }
            public class Outer {
                public static class Nested {
                    public static func Value() -> int { return 42 }
                }
                public func Value() -> int { return 7 }
            }
            """;
        var assembly = Emit(source);
        var generic = assembly.GetType("Generic`1")!;
        Assert.True(generic.IsGenericTypeDefinition);
        Assert.Equal(42, generic.MakeGenericType(typeof(int)).GetMethod("Identity")!.Invoke(null, [42]));
        var outer = assembly.GetType("Outer")!;
        Assert.False(outer.IsAbstract);
        Assert.True(outer.IsSealed); // Ordinary Raven classes are closed by default.
        var nested = outer.GetNestedType("Nested")!;
        Assert.True(nested.IsNestedPublic && nested.IsAbstract && nested.IsSealed);
        Assert.Equal(42, nested.GetMethod("Value")!.Invoke(null, null));
        Assert.Equal(7, outer.GetMethod("Value")!.Invoke(Activator.CreateInstance(outer), null));
    }

    private static Assembly Emit(string source)
    {
        var compilation = Compilation.Create("StaticTypes" + Guid.NewGuid().ToString("N"), [SyntaxTree.ParseText(source)],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithOptimizationLevel(OptimizationLevel.Release));
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        return Assembly.Load(output.ToArray());
    }
}
