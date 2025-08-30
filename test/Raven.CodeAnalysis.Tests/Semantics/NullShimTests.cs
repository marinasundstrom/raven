using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class NullShimTests : CompilationTestBase
{
    [Fact]
    public void NullShimType_EmittedInUnionAttribute()
    {
        var source = """
class C {
    M(x: string | null) -> unit { }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        var assembly = Assembly.Load(peStream.ToArray());
        Assert.NotNull(assembly.GetType("Null"));

        var parameter = assembly.GetType("C")!.GetMethod("M")!.GetParameters()[0];
        var attr = parameter.GetCustomAttributesData().Single(a => a.AttributeType.Name == "TypeUnionAttribute");
        var attrTypes = ((IEnumerable<CustomAttributeTypedArgument>)attr.ConstructorArguments[0].Value!)
            .Select(a => (Type)a.Value!);
        Assert.Contains(attrTypes, t => t.Name == "Null");
    }

    [Fact]
    public void NoUnion_NoShimTypesEmitted()
    {
        var source = """
class C {
    M(x: string) -> unit { }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        var assembly = Assembly.Load(peStream.ToArray());
        Assert.Null(assembly.GetType("Null"));
        Assert.Null(assembly.GetType("TypeUnionAttribute"));
    }

    [Fact]
    public void UnionWithoutNull_EmitsTypeUnionAttributeOnly()
    {
        var source = """
class C {
    M(x: string | int) -> unit { }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        var assembly = Assembly.Load(peStream.ToArray());
        Assert.Null(assembly.GetType("Null"));

        var parameter = assembly.GetType("C")!.GetMethod("M")!.GetParameters()[0];
        var attr = parameter.GetCustomAttributesData().Single(a => a.AttributeType.Name == "TypeUnionAttribute");
        var attrTypes = ((IEnumerable<CustomAttributeTypedArgument>)attr.ConstructorArguments[0].Value!)
            .Select(a => (Type)a.Value!);
        Assert.DoesNotContain(attrTypes, t => t.Name == "Null");
    }
}
