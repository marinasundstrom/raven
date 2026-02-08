using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.CodeGen;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public sealed class RuntimeSymbolResolverTests
{
    [Fact]
    public void GenericMethodInvocation_UsesMethodOwnedTypeParameter()
    {
        const string source = """
import System.Threading.Tasks.*

func Wrap<T>(value: T) -> Task<T> {
    return Task.FromResult(value)
}
""";

        var calls = CaptureMethodCalls(
            source,
            static generator => generator.MethodSymbol.Name == "Wrap" && generator.MethodSymbol.IsGenericMethod,
            OutputKind.ConsoleApplication);

        var fromResult = calls.First(static call => call.Name == "FromResult");
        var genericParameter = GetCarrierGenericArguments(fromResult).First(static arg => arg.IsGenericParameter);

        Assert.True(genericParameter.IsGenericMethodParameter);
        Assert.False(genericParameter.IsGenericTypeParameter);
    }

    [Fact]
    public void AsyncGenericKickoff_UsesMethodOwnedTypeParameter()
    {
        const string source = """
import System.Console.*
import System.Threading.Tasks.*

async func Compute<T>(value: T) -> Task<T> {
    WriteLine("before")
    await Task.Delay(1)
    return value
}
""";

        var calls = CaptureMethodCalls(
            source,
            static generator =>
                generator.MethodSymbol.Name == "Compute" &&
                generator.MethodSymbol.IsGenericMethod &&
                generator.MethodSymbol.ContainingType?.Name == "Program",
            OutputKind.ConsoleApplication);

        var carrierCall = calls.First(static call =>
            call.Name == "FromResult" ||
            (call.DeclaringType is not null &&
             call.DeclaringType.Name.Contains("AsyncTaskMethodBuilder", StringComparison.Ordinal)));

        var genericParameter = GetCarrierGenericArguments(carrierCall).First(static arg => arg.IsGenericParameter);

        Assert.True(genericParameter.IsGenericMethodParameter);
        Assert.False(genericParameter.IsGenericTypeParameter);
    }

    [Fact]
    public void ResultTryGetOkCall_UsesClosedNestedOutType()
    {
        var (resultReference, path) = CreateRavenCoreResultReference();
        try
        {
        var compilation = Compilation.Create(
                "runtime_resolver_result_trygetok",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences([.. TestMetadataReferences.Default, resultReference]);

        var resultDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Result`2"));
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var exceptionType = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Exception"));
        var constructedResult = Assert.IsAssignableFrom<INamedTypeSymbol>(
            resultDefinition.Construct(intType, exceptionType));
        var tryGetOkSymbol = Assert.Single(constructedResult.GetMembers("TryGetOk").OfType<IMethodSymbol>());

        var codeGenerator = new CodeGenerator(compilation)
        {
            ILBuilderFactory = ReflectionEmitILBuilderFactory.Instance
        };

        var tryGetOk = codeGenerator.RuntimeSymbolResolver.GetMethodInfo(tryGetOkSymbol);
        Assert.False(tryGetOk.ContainsGenericParameters);

        var outParameter = Assert.Single(tryGetOk.GetParameters());
        Assert.True(outParameter.ParameterType.IsByRef);

        var outElementType = outParameter.ParameterType.GetElementType();
        Assert.NotNull(outElementType);
        Assert.False(outElementType!.ContainsGenericParameters);
        Assert.Contains("+Ok", outElementType.FullName ?? outElementType.Name, StringComparison.Ordinal);
        }
        finally
        {
            if (File.Exists(path))
                File.Delete(path);
        }
    }

    [Fact]
    public void AwaitUnsafeOnCompleted_UsesClosedAwaiterTypeArguments()
    {
        const string source = """
import System.Collections.Generic.*
import System.Threading.*
import System.Threading.Tasks.*

async func Compute() -> Task<Dictionary<string, int>> {
    val values: Task<Dictionary<string, int>> = Task.FromResult(Dictionary<string, int>())
    return await values
}
""";

        var calls = CaptureMethodCalls(
            source,
            static generator =>
                generator.MethodSymbol.Name == "MoveNext" &&
                generator.MethodSymbol.ContainingType is SynthesizedAsyncStateMachineTypeSymbol stateMachine &&
                stateMachine.AsyncMethod.Name == "Compute",
            OutputKind.DynamicallyLinkedLibrary);

        var awaitUnsafeOnCompleted = calls.First(static call =>
            string.Equals(call.Name, "AwaitUnsafeOnCompleted", StringComparison.Ordinal));

        Assert.True(awaitUnsafeOnCompleted.IsGenericMethod);
        var genericArguments = awaitUnsafeOnCompleted.GetGenericArguments();
        Assert.True(genericArguments.Length >= 1);

        var awaiterType = genericArguments[0];
        Assert.False(awaiterType.ContainsGenericParameters);
        Assert.Contains("TaskAwaiter`1", awaiterType.ToString(), StringComparison.Ordinal);
        Assert.Contains("Dictionary`2[System.String,System.Int32]", awaiterType.ToString(), StringComparison.Ordinal);
    }

    private static MethodInfo[] CaptureMethodCalls(
        string source,
        Func<MethodGenerator, bool> predicate,
        OutputKind outputKind)
    {
        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("runtime_resolver", new CompilationOptions(outputKind))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.Default);

        _ = compilation.GetSpecialType(SpecialType.System_Object);

        var recorder = new RecordingILBuilderFactory(ReflectionEmitILBuilderFactory.Instance, predicate);
        var codeGenerator = new CodeGenerator(compilation)
        {
            ILBuilderFactory = recorder
        };

        using var peStream = new MemoryStream();
        codeGenerator.Emit(peStream, pdbStream: null);

        return recorder.CapturedInstructions!
            .Select(instruction => instruction.Operand.Value as MethodInfo)
            .Where(static method => method is not null)
            .ToArray()!;
    }

    private static IEnumerable<Type> GetCarrierGenericArguments(MethodInfo method)
    {
        if (method.IsGenericMethod && !method.IsGenericMethodDefinition)
            return method.GetGenericArguments();

        if (method.DeclaringType is { IsGenericType: true } declaringType)
            return declaringType.GetGenericArguments();

        return Array.Empty<Type>();
    }

    private static (MetadataReference Reference, string Path) CreateRavenCoreResultReference()
    {
        var coreDirectory = Path.GetFullPath(Path.Combine(
            "..", "..", "..", "..", "..", "src", "Raven.Core"));
        var optionTree = SyntaxTree.ParseText(File.ReadAllText(Path.Combine(coreDirectory, "Option.rav")));
        var resultTree = SyntaxTree.ParseText(File.ReadAllText(Path.Combine(coreDirectory, "Result.rav")));
        var compilation = Compilation.Create(
            "raven-core-result-runtime-resolver-fixture",
            [optionTree, resultTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var stream = new MemoryStream();
        var emitResult = compilation.Emit(stream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        var path = Path.Combine(Path.GetTempPath(), $"raven-core-result-runtime-resolver-{Guid.NewGuid():N}.dll");
        File.WriteAllBytes(path, stream.ToArray());
        return (MetadataReference.CreateFromFile(path), path);
    }
}
