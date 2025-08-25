using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class FieldInitializationTests
{
    [Fact]
    public void InstanceAndStaticFieldInitializers_AreEmitted()
    {
        var code = """
class Foo {
    var x: int = 42
    static var y: int = 100
    public X: int { get => x; }
    public static Y: int { get => y; }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveLatestInstalledVersion();
        var runtimePath = TargetFrameworkResolver.GetRuntimeDll(version);

        MetadataReference[] references = [
            MetadataReference.CreateFromFile(runtimePath)
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        peStream.Seek(0, SeekOrigin.Begin);

        var resolver = new PathAssemblyResolver(references.Select(r => ((PortableExecutableReference)r).FilePath));
        using var mlc = new MetadataLoadContext(resolver);

        var assembly = mlc.LoadFromStream(peStream);
        var type = assembly.GetType("Foo", true);
        var instance = Activator.CreateInstance(type!);
        var xProp = type!.GetProperty("X");
        var yProp = type!.GetProperty("Y");

        Assert.Equal(42, (int)xProp!.GetValue(instance)!);
        Assert.Equal(100, (int)yProp!.GetValue(null)!);
    }
}
