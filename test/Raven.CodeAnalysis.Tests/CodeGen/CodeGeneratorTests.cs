using System.IO;
using System.Linq;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;

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

        using var mlc = CreateMetadataLoadContext(references);

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

        using var mlc = CreateMetadataLoadContext(references);

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
        var image = peStream.ToArray();

        using var mlc = CreateMetadataLoadContext(references);
        var assembly = mlc.LoadFromStream(new MemoryStream(image));

        var fooType = assembly.GetType("Foo", throwOnError: true)!;
        var interfaceType = assembly.GetType("System.IDisposable", throwOnError: true)!;

        using var peReader = new PEReader(new MemoryStream(image));
        var reader = peReader.GetMetadataReader();

        var fooHandle = reader.TypeDefinitions
            .First(handle =>
            {
                var type = reader.GetTypeDefinition(handle);
                return reader.GetString(type.Name) == "Foo" && reader.GetString(type.Namespace) == string.Empty;
            });

        var fooDefinition = reader.GetTypeDefinition(fooHandle);

        var disposeMethod = fooDefinition.GetMethods()
            .Select(handle => (handle, definition: reader.GetMethodDefinition(handle)))
            .First(tuple => reader.GetString(tuple.definition.Name) == "Dispose");

        var methodImplHandles = fooDefinition.GetMethodImplementations();
        Assert.Single(methodImplHandles);

        var methodImpl = reader.GetMethodImplementation(methodImplHandles.Single());
        Assert.Equal(disposeMethod.handle, methodImpl.MethodBody);

        Assert.Equal(HandleKind.MemberReference, methodImpl.MethodDeclaration.Kind);
        var memberReference = reader.GetMemberReference((MemberReferenceHandle)methodImpl.MethodDeclaration);
        Assert.Equal("Dispose", reader.GetString(memberReference.Name));

        Assert.Equal(HandleKind.TypeReference, memberReference.Parent.Kind);
        var typeReference = reader.GetTypeReference((TypeReferenceHandle)memberReference.Parent);
        Assert.Equal("System", reader.GetString(typeReference.Namespace));
        Assert.Equal("IDisposable", reader.GetString(typeReference.Name));
    }

    private static MetadataLoadContext CreateMetadataLoadContext(MetadataReference[] references)
    {
        var assemblyPaths = references
            .OfType<PortableExecutableReference>()
            .Select(r => r.FilePath)
            .Where(p => !string.IsNullOrEmpty(p))
            .ToArray();

        var resolver = new PathAssemblyResolver(assemblyPaths!);

        static bool IsMatch(string? fileName, string expected) =>
            string.Equals(fileName, expected, StringComparison.OrdinalIgnoreCase);

        string? coreAssemblyFileName = assemblyPaths
            .Select(Path.GetFileName)
            .FirstOrDefault(file => IsMatch(file, "System.Private.CoreLib.dll"));

        coreAssemblyFileName ??= assemblyPaths
            .Select(Path.GetFileName)
            .FirstOrDefault(file => IsMatch(file, "System.Runtime.dll"));

        if (coreAssemblyFileName is not null)
        {
            return new MetadataLoadContext(resolver, Path.GetFileNameWithoutExtension(coreAssemblyFileName));
        }

        return new MetadataLoadContext(resolver);
    }
}
