using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class LiteralUnionEmissionTests
{
    [Fact]
    public void LiteralUnion_EmitsLiteralInAttribute()
    {
        var source = """
class C {
    M(x: int | "yes") -> unit { }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("lib", [tree], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        var assembly = Assembly.Load(peStream.ToArray());
        var parameter = assembly.GetType("C")!
            .GetMethod("M", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)!
            .GetParameters()[0];
        var attr = parameter.GetCustomAttributesData().Single(a => a.AttributeType.Name == "TypeUnionAttribute");
        var values = ((IEnumerable<CustomAttributeTypedArgument>)attr.ConstructorArguments[0].Value!)
            .Select(a => a.Value).ToArray();
        Assert.Contains(values, v => v is Type t && t == typeof(int));
        Assert.Contains(values, v => v is Type t && t == typeof(string));
    }

    [Fact]
    public void EmbedCoreTypes_ProducesTypeUnionAttribute()
    {
        var source = """
class C {
    M(x: int | string) -> unit { }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithEmbedCoreTypes(true);
        var compilation = Compilation.Create("lib", [tree], options)
            .AddReferences(TestMetadataReferences.Default);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        var assembly = Assembly.Load(peStream.ToArray());
        var attrType = assembly.GetType("System.Runtime.CompilerServices.TypeUnionAttribute");
        Assert.NotNull(attrType);
        Assert.True(attrType!.IsPublic);
    }
}
