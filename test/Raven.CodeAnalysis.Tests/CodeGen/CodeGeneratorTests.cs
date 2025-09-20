using System.Reflection;

using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class CodeGeneratorTests
{
    [Fact]
    public void Emit_ShouldGenerateClass()
    {
        var code = """
class Foo {
    Test() -> unit {
        return;
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);

        var runtimePath = TargetFrameworkResolver.GetRuntimeDll(version);

        MetadataReference[] references = [
                MetadataReference.CreateFromFile(runtimePath)];

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

        Assert.NotNull(assembly.GetType("Foo", true));
    }

    [Fact]
    public void Emit_ShouldAlwaysIncludeUnitType()
    {
        var code = """
func main() {
    let x = if true {
        42
    } else {
        ()
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);

        var runtimePath = TargetFrameworkResolver.GetRuntimeDll(version);

        MetadataReference[] references = [
                MetadataReference.CreateFromFile(runtimePath)];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success);
    }

    [Fact]
    public void Emit_ClassImplementingInterface_RegistersInterfaceImplementation()
    {
        var code = """
interface IFoo {
}

class Foo : IFoo {
    Foo() -> unit {
        return;
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);

        var runtimePath = TargetFrameworkResolver.GetRuntimeDll(version);

        MetadataReference[] references = [
                MetadataReference.CreateFromFile(runtimePath)];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        peStream.Seek(0, SeekOrigin.Begin);

        var resolver = new PathAssemblyResolver(references.Select(r => ((PortableExecutableReference)r).FilePath));
        using var mlc = new MetadataLoadContext(resolver);

        var assembly = mlc.LoadFromStream(peStream);

        var fooType = assembly.GetType("Foo", throwOnError: true)!;
        var interfaceType = assembly.GetType("IFoo", throwOnError: true)!;

        Assert.True(interfaceType.IsInterface);
        Assert.Contains(interfaceType, fooType.GetInterfaces());
    }

    [Fact]
    public void Emit_ClassWithInterfaceMethod_EmitsInterfaceContract()
    {
        var code = """
import System.*;

class Foo : IDisposable {
    Foo() -> unit {
        return;
    }

    Dispose() -> unit {
        return;
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);

        var runtimePath = TargetFrameworkResolver.GetRuntimeDll(version);

        MetadataReference[] references = [
                MetadataReference.CreateFromFile(runtimePath)];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        peStream.Seek(0, SeekOrigin.Begin);

        var resolver = new PathAssemblyResolver(references.Select(r => ((PortableExecutableReference)r).FilePath));
        using var mlc = new MetadataLoadContext(resolver);

        var assembly = mlc.LoadFromStream(peStream);

        var fooType = assembly.GetType("Foo", throwOnError: true)!;
        var interfaceType = assembly.GetReferencedAssemblies()
            .Select(x => Assembly.Load(x))
            .Select(x => x.GetType("System.IDisposable", throwOnError: false))
            .FirstOrDefault(x => x is not null)!;

        var map = fooType.GetInterfaceMap(interfaceType);
        Assert.Single(map.InterfaceMethods);
        Assert.Equal("Dispose", map.InterfaceMethods[0].Name);
        Assert.Equal("Dispose", map.TargetMethods[0].Name);
        Assert.True(map.InterfaceMethods[0].IsAbstract);
        Assert.True(map.TargetMethods[0].IsFinal);
    }
}
