using System;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class UnionEmissionTests
{
    [Fact]
    public void CommonBaseClass_WithNull_UsesBaseTypeAndNullable()
    {
        var source = """
class Base {}
class A : Base {}
class B : Base {}
class C {
    M(x: A | B | null) -> unit { }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("lib", [tree], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        var assembly = Assembly.Load(peStream.ToArray());
        var parameter = assembly.GetType("C")!.GetMethod("M")!.GetParameters()[0];

        Assert.Equal(assembly.GetType("Base"), parameter.ParameterType);
        Assert.Contains(parameter.GetCustomAttributesData(), a => a.AttributeType.Name == "NullableAttribute");
    }

    [Fact]
    public void CommonInterface_UsesInterfaceInSignature()
    {
        var source = """
interface IThing {}
class A : IThing {}
class B : IThing {}
class C {
    M(x: A | B) -> unit { }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("lib", [tree], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        var assembly = Assembly.Load(peStream.ToArray());
        var parameter = assembly.GetType("C")!.GetMethod("M")!.GetParameters()[0];

        Assert.Equal(assembly.GetType("IThing"), parameter.ParameterType);
    }

    [Fact]
    public void ValueTypeWithNull_UsesNullableValueType()
    {
        var source = """
class C {
    M(x: int | null) -> unit { }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("lib", [tree], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        var assembly = Assembly.Load(peStream.ToArray());
        var parameter = assembly.GetType("C")!.GetMethod("M")!.GetParameters()[0];

        Assert.True(parameter.ParameterType.IsGenericType);
        Assert.Equal(typeof(Nullable<>), parameter.ParameterType.GetGenericTypeDefinition());
        Assert.Equal(typeof(int), parameter.ParameterType.GetGenericArguments()[0]);
        Assert.DoesNotContain(parameter.GetCustomAttributesData(), a => a.AttributeType.Name == "NullableAttribute");
    }
}

