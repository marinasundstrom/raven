using System;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public sealed class DelegateDeclarationCodeGenTests
{
    [Fact]
    public void DelegateDeclaration_EmitsClrDelegateShape()
    {
        const string code = """
public delegate Transformer(ref value: int, out result: string) -> bool
public delegate UnitHandler()
""";

        using var metadataContext = Emit(code, out var assembly);

        var transformer = assembly.GetType("Transformer", throwOnError: true)!;
        var unitHandler = assembly.GetType("UnitHandler", throwOnError: true)!;

        Assert.True(transformer.IsSealed);
        Assert.False(transformer.IsAbstract);
        Assert.True(transformer.IsClass);
        Assert.Equal("System.MulticastDelegate", transformer.BaseType?.FullName);

        var constructor = transformer.GetConstructors(BindingFlags.Public | BindingFlags.Instance).Single();
        var constructorParameters = constructor.GetParameters();
        Assert.Equal("System.Object", constructorParameters[0].ParameterType.FullName);
        Assert.Equal("System.IntPtr", constructorParameters[1].ParameterType.FullName);

        var invoke = transformer.GetMethod("Invoke", BindingFlags.Public | BindingFlags.Instance);
        Assert.NotNull(invoke);
        Assert.Equal("System.Boolean", invoke!.ReturnType.FullName);

        var parameters = invoke.GetParameters();
        Assert.Equal(2, parameters.Length);
        Assert.True(parameters[0].ParameterType.IsByRef);
        Assert.False(parameters[0].IsOut);
        Assert.True(parameters[1].ParameterType.IsByRef);
        Assert.True(parameters[1].IsOut);

        var unitInvoke = unitHandler.GetMethod("Invoke", BindingFlags.Public | BindingFlags.Instance);
        Assert.NotNull(unitInvoke);
        Assert.Equal("System.Void", unitInvoke!.ReturnType.FullName);
    }

    [Fact]
    public void DelegateDeclaration_LambdaCanInstantiateAndInvokeEmittedDelegate()
    {
        const string code = """
delegate Test(a: int, b: int) -> int

public class Program {
    public static func Run() -> int {
        val d: Test = (a, b) => a + b
        return d(2, 3)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(syntaxTree);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, compilation.References);
        var program = loaded.Assembly.GetType("Program", throwOnError: true)!;
        var run = program.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;

        Assert.Equal(5, run.Invoke(null, []));
    }

    [Fact]
    public void DelegateDeclaration_DefaultAccessibility_IsEmittedCorrectly()
    {
        const string code = """
delegate DefaultHandler()
public delegate PublicHandler()

public class Container {
    delegate DefaultNested()
    public delegate PublicNested()
}

public interface IContainer {
    delegate InterfaceNested()
}
""";

        using var metadataContext = Emit(code, out var assembly);

        var defaultHandler = assembly.GetType("DefaultHandler", throwOnError: true)!;
        var publicHandler = assembly.GetType("PublicHandler", throwOnError: true)!;
        var container = assembly.GetType("Container", throwOnError: true)!;
        var interfaceContainer = assembly.GetType("IContainer", throwOnError: true)!;
        var flags = BindingFlags.Public | BindingFlags.NonPublic;

        Assert.True(defaultHandler.IsPublic);
        Assert.True(publicHandler.IsPublic);
        AssertNestedVisibility(container, "DefaultNested", TypeAttributes.NestedPrivate, flags);
        AssertNestedVisibility(container, "PublicNested", TypeAttributes.NestedPublic, flags);
        AssertNestedVisibility(interfaceContainer, "InterfaceNested", TypeAttributes.NestedPublic, flags);
    }

    [Fact]
    public void DelegateDeclaration_EmitsCustomAttributes()
    {
        const string code = """
import System.*

[Obsolete("callback")]
public delegate Callback(value: int) -> string
""";

        using var metadataContext = Emit(code, out var assembly);

        var callback = assembly.GetType("Callback", throwOnError: true)!;
        var attribute = Assert.Single(
            callback.GetCustomAttributesData(),
            static attribute => attribute.AttributeType.Name == "ObsoleteAttribute");

        Assert.Equal("callback", Assert.Single(attribute.ConstructorArguments).Value);
    }

    private static void AssertNestedVisibility(Type container, string nestedName, TypeAttributes expected, BindingFlags flags)
    {
        var nestedType = container.GetNestedType(nestedName, flags);
        Assert.NotNull(nestedType);
        Assert.Equal(expected, nestedType!.Attributes & TypeAttributes.VisibilityMask);
    }

    private static MetadataLoadContext Emit(string code, out Assembly assembly)
    {
        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(syntaxTree);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        peStream.Seek(0, SeekOrigin.Begin);
        var resolver = new PathAssemblyResolver(compilation.References
            .OfType<PortableExecutableReference>()
            .Select(r => r.FilePath)
            .Where(p => p is not null)!);

        var metadataContext = new MetadataLoadContext(resolver);
        assembly = metadataContext.LoadFromStream(peStream);
        return metadataContext;
    }

    private static Compilation CreateCompilation(SyntaxTree syntaxTree, CompilationOptions? options = null)
    {
        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var runtimePath = TargetFrameworkResolver.GetRuntimeDll(version);

        var compilation = Compilation.Create("test", options ?? new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(MetadataReference.CreateFromFile(runtimePath));

        return compilation;
    }
}
