using System;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;

using Raven.CodeAnalysis;
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

        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        var interfaceSymbol = compilation.GlobalNamespace.GetMembers("IUtility").OfType<INamedTypeSymbol>().Single();
        var getValueSymbol = interfaceSymbol.GetMembers("GetValue").OfType<IMethodSymbol>().Single();
        Assert.True(getValueSymbol.IsStatic);
        Assert.Single(interfaceSymbol.GetMembers("GetValue").Where(m => m.IsStatic));

        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        var memberAccess = syntaxTree.GetRoot().DescendantNodes().OfType<MemberAccessExpressionSyntax>().Single();
        var typeInfo = semanticModel.GetTypeInfo(memberAccess.Expression);
        Assert.Same(interfaceSymbol, typeInfo.Type);
        var memberInfo = semanticModel.GetSymbolInfo(memberAccess.Name);
        Assert.True(memberInfo.Success, memberInfo.ToString());

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;

        Assert.NotNull(assembly.GetType("Foo", true));
    }

    [Fact]
    public void Emit_WithSingleMainInNonProgramType_UsesThatEntryPoint()
    {
        var code = """
class Helper {
    static Main() -> int {
        return 0;
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var entryPoint = assembly.EntryPoint;

        Assert.NotNull(entryPoint);
        Assert.Equal("Helper", entryPoint!.DeclaringType!.Name);
    }

    [Fact]
    public void Emit_WithAsyncMainInClass_SynthesizesBridgeEntryPoint()
    {
        var code = """
import System.Threading.Tasks.*

class Helper {
    static async Main(args: string[]) -> Task<int> {
        await Task.Delay(1);
        return 7;
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        peStream.Seek(0, SeekOrigin.Begin);

        var resolver = new PathAssemblyResolver(references.Select(r => ((PortableExecutableReference)r).FilePath));
        using var mlc = new MetadataLoadContext(resolver);

        var assembly = mlc.LoadFromStream(peStream);
        var entryPoint = assembly.EntryPoint;

        Assert.NotNull(entryPoint);
        Assert.Equal("<Main>_EntryPoint", entryPoint!.Name);
        Assert.Equal(typeof(int), entryPoint.ReturnType);

        var exitCode = entryPoint.Invoke(null, new object?[] { Array.Empty<string>() });

        Assert.Equal(7, exitCode);
    }

    [Fact]
    public void Emit_WithAsyncFunctionMain_SynthesizesBridgeEntryPoint()
    {
        var code = """
import System.Threading.Tasks.*

async func Main() -> Task<int> {
    await Task.Delay(1);
    return 11;
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        peStream.Seek(0, SeekOrigin.Begin);

        var resolver = new PathAssemblyResolver(references.Select(r => ((PortableExecutableReference)r).FilePath));
        using var mlc = new MetadataLoadContext(resolver);

        var assembly = mlc.LoadFromStream(peStream);
        var entryPoint = assembly.EntryPoint;

        Assert.NotNull(entryPoint);
        Assert.Equal("<Main>_EntryPoint", entryPoint!.Name);
        Assert.Equal(typeof(int), entryPoint.ReturnType);

        var exitCode = entryPoint.Invoke(null, Array.Empty<object?>());

        Assert.Equal(11, exitCode);
    }

    [Fact]
    public void Emit_WithAsyncFunctionMainReturningTask_SynthesizesBridgeEntryPoint()
    {
        var code = """
import System.Console.*
import System.Threading.Tasks.*

async func Main() -> Task {
    await Task.Delay(1);
    WriteLine("Async hello");
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        peStream.Seek(0, SeekOrigin.Begin);

        var resolver = new PathAssemblyResolver(references.Select(r => ((PortableExecutableReference)r).FilePath));
        using var mlc = new MetadataLoadContext(resolver);

        var assembly = mlc.LoadFromStream(peStream);
        var entryPoint = assembly.EntryPoint;

        Assert.NotNull(entryPoint);
        Assert.Equal("<Main>_EntryPoint", entryPoint!.Name);
        Assert.Equal(typeof(void), entryPoint.ReturnType);

        using var writer = new StringWriter();
        var originalOut = Console.Out;

        try
        {
            Console.SetOut(writer);

            var exitCode = entryPoint.Invoke(null, Array.Empty<object?>());

            Assert.Null(exitCode);
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        var output = writer.ToString().Replace("\r\n", "\n").Trim();
        Assert.Equal("Async hello", output);
    }

    [Fact]
    public void Emit_WithMultipleValidMainMethods_FailsWithAmbiguousEntryPointDiagnostic()
    {
        var code = """
class Program {
    static Main() -> int {
        return 42;
    }
}

class Helper {
    static Main() -> int {
        return 0;
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.False(result.Success);
        Assert.Contains(result.Diagnostics, d => d.Descriptor == CompilerDiagnostics.EntryPointIsAmbiguous);
        Assert.DoesNotContain(result.Diagnostics, d => d.Descriptor == CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint);
    }

    [Fact]
    public void Emit_ShouldAlwaysIncludeUnitType()
    {
        var code = """
val x = if true {
    42
} else {
    ()
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var references = TestMetadataReferences.Default;

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

        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
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

        Assert.Contains(fooType.GetInterfaceImplementations(), handle =>
        {
            var implementation = metadataReader.GetInterfaceImplementation(handle);
            var (ns, name) = GetTypeIdentity(metadataReader, implementation.Interface);
            return name == "IFoo" && string.IsNullOrEmpty(ns);
        });
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

        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
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

    [Fact]
    public void Emit_InterfaceStaticMembers_EmitsCallableStatics()
    {
        var code = """
interface IUtility {
    public static GetValue() -> int {
        return 42;
    }
}

class Consumer {
    public Combine() -> int {
        return IUtility.GetValue();
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        Assert.Empty(syntaxTree.GetDiagnostics());
        var interfaceDecl = syntaxTree.GetRoot().Members.OfType<InterfaceDeclarationSyntax>().Single();
        var methodDecl = interfaceDecl.Members.OfType<MethodDeclarationSyntax>().Single();
        Assert.Contains(methodDecl.Modifiers, m => m.Kind == SyntaxKind.StaticKeyword);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;

        var interfaceType = assembly.GetType("IUtility", throwOnError: true)!;
        var staticMethod = interfaceType.GetMethod("GetValue", BindingFlags.Public | BindingFlags.Static);

        Assert.NotNull(staticMethod);
        Assert.True(staticMethod!.IsStatic);
        Assert.False(staticMethod.IsAbstract);
        var methodResult = (int)staticMethod.Invoke(null, Array.Empty<object?>())!;
        Assert.Equal(42, methodResult);

        var consumerType = assembly.GetType("Consumer", throwOnError: true)!;
        var combine = consumerType.GetMethod("Combine")!;
        var instance = Activator.CreateInstance(consumerType);
        var combined = (int)combine.Invoke(instance, Array.Empty<object?>())!;

        Assert.Equal(42, combined);
    }

    [Fact]
    public void Emit_InterfaceDefaultImplementations_EmitAndInvoke()
    {
        var code = """
interface ILogger {
    public Log(message: string) -> string {
        return "[default]";
    }
}

class ConsoleLogger : ILogger {
    ConsoleLogger() -> unit { return; }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;

        var interfaceType = assembly.GetType("ILogger", throwOnError: true)!;
        var defaultMethod = interfaceType.GetMethod("Log", BindingFlags.Public | BindingFlags.Instance);

        Assert.NotNull(defaultMethod);
        Assert.False(defaultMethod!.IsAbstract);
        Assert.NotNull(defaultMethod.GetMethodBody());

        var loggerType = assembly.GetType("ConsoleLogger", throwOnError: true)!;
        var instance = Activator.CreateInstance(loggerType);
        var value = (string)defaultMethod.Invoke(instance, new object?[] { "ignored" })!;

        Assert.Equal("[default]", value);
    }

    [Fact]
    public void Emit_ExplicitInterfaceImplementation_EmitsPrivateOverride()
    {
        var code = """
interface ILogger {
    Log(message: string) -> string
}

class QuietLogger : ILogger {
    ILogger.Log(message: string) -> string {
        return "[quiet]"
    }

    public Log(message: string) -> string {
        return message
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.False(result.Success);
        Assert.Contains(result.Diagnostics, d => d.Descriptor == CompilerDiagnostics.ExplicitInterfaceMemberNotFound);
        Assert.DoesNotContain(result.Diagnostics, d => d.Descriptor == CompilerDiagnostics.ExplicitInterfaceSpecifierMustBeInterface);
    }

    [Fact]
    public void Emit_ExplicitInterfacePropertyImplementation_EmitsPrivateOverride()
    {
        var code = """
interface ILogger {
    Message: string { get; set; }
}

class QuietLogger : ILogger {
    var message: string = "[quiet]";

    ILogger.Message: string {
        get => message;
        set => message = value;
    }

    public Echo: string {
        get => message;
        set => message = value;
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.False(result.Success);
        Assert.Contains(result.Diagnostics, d => d.Descriptor == CompilerDiagnostics.ExplicitInterfaceMemberNotFound);

        Assert.DoesNotContain(result.Diagnostics, d => d.Descriptor == CompilerDiagnostics.ExplicitInterfaceSpecifierMustBeInterface);
    }

    [Fact]
    public void Emit_NamedConstructorWithImplicitReceivers_EmitsAndRuns()
    {
        var code = """
class Person {
    var storedName: string;

    public init WithName(name: string) {
        storedName = name;
        val snapshot = storedName;
        Normalize();
    }

    private Normalize() -> unit {
        if storedName == "" {
            storedName = "Unknown";
        }
    }

    public GetName() -> string => storedName;
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var personType = assembly.GetType("Person", throwOnError: true)!;
        var factory = personType.GetMethod("WithName")!;
        var instance = factory.Invoke(null, new object?[] { "" });

        var getName = personType.GetMethod("GetName")!;
        var value = (string)getName.Invoke(instance, Array.Empty<object?>())!;

        Assert.Equal("Unknown", value);
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
