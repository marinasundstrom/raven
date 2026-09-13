using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class UnsignedArrayElementTests
{
    [Theory]
    [InlineData("byte", 255L)]
    [InlineData("ushort", 65535L)]
    [InlineData("char", 65535L)]
    [InlineData("uint", 4294967295L)]
    public void WideningUnsignedArrayElementPreservesMagnitude(string typeName, long expected)
    {
        var source = "class Probe { func Read(values: " + typeName + "[]) -> long { return (long)values[0] } }";
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("ArrayElements", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(references);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics.Select(d => d.ToString())));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var type = loaded.Assembly.GetType("Probe", true)!;
        var method = type.GetMethod("Read", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance)!;
        object values = typeName switch
        {
            "byte" => new byte[] { byte.MaxValue },
            "ushort" => new ushort[] { ushort.MaxValue },
            "char" => new char[] { char.MaxValue },
            _ => new uint[] { uint.MaxValue }
        };
        Assert.Equal(expected, method.Invoke(Activator.CreateInstance(type), new[] { values }));
    }
}
