using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;

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
import System.*

class Foo : IDisposable {
    init() {

    }

    Dispose() -> unit {

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

        using var peReader = new PEReader(peStream, PEStreamOptions.PrefetchEntireImage);
        var metadataReader = peReader.GetMetadataReader();

        var fooType = metadataReader.TypeDefinitions
            .Select(metadataReader.GetTypeDefinition)
            .First(typeDefinition =>
            {
                var name = metadataReader.GetString(typeDefinition.Name);
                if (name != "Foo")
                    return false;

                var ns = metadataReader.GetString(typeDefinition.Namespace);
                return string.IsNullOrEmpty(ns);
            });

        var disposeHandle = fooType.GetMethods()
            .First(methodHandle =>
            {
                var methodDefinition = metadataReader.GetMethodDefinition(methodHandle);
                return metadataReader.GetString(methodDefinition.Name) == "Dispose";
            });

        var disposeMethod = metadataReader.GetMethodDefinition(disposeHandle);
        Assert.True(disposeMethod.Attributes.HasFlag(MethodAttributes.Virtual));
        Assert.True(disposeMethod.Attributes.HasFlag(MethodAttributes.Final));

        Assert.Contains(fooType.GetInterfaceImplementations(), handle =>
        {
            var implementation = metadataReader.GetInterfaceImplementation(handle);
            var (ns, name) = GetTypeIdentity(metadataReader, implementation.Interface);
            return name == "IDisposable" && ns == "System";
        });

        Assert.Contains(fooType.GetMethodImplementations(), handle =>
        {
            var implementation = metadataReader.GetMethodImplementation(handle);

            if (!IsMethod(metadataReader, implementation.MethodBody, "Foo", "Dispose"))
                return false;

            return IsMethod(metadataReader, implementation.MethodDeclaration, "System.IDisposable", "Dispose");
        });
    }

    private static bool IsMethod(MetadataReader metadataReader, EntityHandle handle, string containingTypeName, string methodName)
    {
        return handle.Kind switch
        {
            HandleKind.MethodDefinition => IsMethodDefinition(metadataReader, (MethodDefinitionHandle)handle, containingTypeName, methodName),
            HandleKind.MemberReference => IsMemberReference(metadataReader, (MemberReferenceHandle)handle, containingTypeName, methodName),
            _ => false,
        };
    }

    private static (string Namespace, string Name) GetTypeIdentity(MetadataReader metadataReader, EntityHandle handle)
    {
        return handle.Kind switch
        {
            HandleKind.TypeDefinition =>
                GetTypeIdentity(metadataReader, metadataReader.GetTypeDefinition((TypeDefinitionHandle)handle)),
            HandleKind.TypeReference =>
                GetTypeIdentity(metadataReader, metadataReader.GetTypeReference((TypeReferenceHandle)handle)),
            _ => (string.Empty, string.Empty)
        };
    }

    private static string BuildQualifiedName(string namespaceName, string typeName)
    {
        if (string.IsNullOrEmpty(namespaceName))
            return typeName;

        return $"{namespaceName}.{typeName}";
    }

    private static (string Namespace, string Name) GetTypeIdentity(MetadataReader metadataReader, TypeDefinition typeDefinition)
    {
        return (metadataReader.GetString(typeDefinition.Namespace), metadataReader.GetString(typeDefinition.Name));
    }

    private static (string Namespace, string Name) GetTypeIdentity(MetadataReader metadataReader, TypeReference typeReference)
    {
        return (metadataReader.GetString(typeReference.Namespace), metadataReader.GetString(typeReference.Name));
    }

    private static bool IsMethodDefinition(MetadataReader metadataReader, MethodDefinitionHandle methodHandle, string containingTypeName, string methodName)
    {
        var methodDefinition = metadataReader.GetMethodDefinition(methodHandle);
        if (metadataReader.GetString(methodDefinition.Name) != methodName)
            return false;

        var typeDefinition = metadataReader.GetTypeDefinition(methodDefinition.GetDeclaringType());
        var qualifiedName = BuildQualifiedName(metadataReader.GetString(typeDefinition.Namespace), metadataReader.GetString(typeDefinition.Name));
        return qualifiedName == containingTypeName;
    }

    private static bool IsMemberReference(MetadataReader metadataReader, MemberReferenceHandle handle, string containingTypeName, string methodName)
    {
        var memberReference = metadataReader.GetMemberReference(handle);
        if (metadataReader.GetString(memberReference.Name) != methodName)
            return false;

        var (ns, name) = GetTypeIdentity(metadataReader, memberReference.Parent);
        var qualifiedName = BuildQualifiedName(ns, name);
        return qualifiedName == containingTypeName;
    }
}
