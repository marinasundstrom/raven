using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public sealed class RuntimeAsyncCodeGenTests
{
    private const int RuntimeAsyncMethodImplBit = 0x2000;

    [Fact]
    public void RuntimeAsyncEnabled_EmitsAsyncMethodImplFlag()
    {
        const string code = """
import System.Threading.Tasks.*

class Program {
    async func Compute() -> Task<int> {
        return await Task.FromResult(1)
    }

    func Sync() -> int {
        return 2
    }
}
""";

        using var loaded = EmitAssembly(code, useRuntimeAsync: true);

        var programType = loaded.Assembly.GetType("Program", throwOnError: true)!;
        var methodFlags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static;
        var asyncMethod = programType.GetMethod("Compute", methodFlags)!;
        var syncMethod = programType.GetMethod("Sync", methodFlags)!;

        Assert.NotEqual(0, ((int)asyncMethod.GetMethodImplementationFlags()) & RuntimeAsyncMethodImplBit);
        Assert.Equal(0, ((int)syncMethod.GetMethodImplementationFlags()) & RuntimeAsyncMethodImplBit);
    }

    [Fact]
    public void RuntimeAsyncDisabled_DoesNotEmitAsyncMethodImplFlag()
    {
        const string code = """
import System.Threading.Tasks.*

class Program {
    async func Compute() -> Task<int> {
        return await Task.FromResult(1)
    }
}
""";

        using var loaded = EmitAssembly(code, useRuntimeAsync: false);

        var programType = loaded.Assembly.GetType("Program", throwOnError: true)!;
        var methodFlags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static;
        var asyncMethod = programType.GetMethod("Compute", methodFlags)!;

        Assert.Equal(0, ((int)asyncMethod.GetMethodImplementationFlags()) & RuntimeAsyncMethodImplBit);
    }

    [Fact]
    public void RuntimeAsyncEnabled_DoesNotEmitAsyncStateMachineType()
    {
        const string code = """
import System.Threading.Tasks.*

class Program {
    async func Compute() -> Task<int> {
        return await Task.FromResult(1)
    }
}
""";

        using var loaded = EmitAssembly(code, useRuntimeAsync: true);

        var generatedTypes = loaded.Assembly.GetTypes();
        Assert.DoesNotContain(
            generatedTypes,
            static t => t.Name.Contains("AsyncStateMachine", StringComparison.Ordinal));
    }

    [Fact]
    public void RuntimeAsyncEnabled_SupportsValueTaskAndConfiguredAwaitShapes()
    {
        const string code = """
import System.Threading.Tasks.*

class Program {
    async func ComputeTaskConfigured() -> Task<int> {
        return await Task.FromResult(1).ConfigureAwait(false)
    }

    async func ComputeTask() -> Task {
        await Task.Delay(1).ConfigureAwait(false)
    }

    async func ComputeValueTaskConfigured() -> Task<int> {
        return await ValueTask.FromResult(2).ConfigureAwait(false)
    }

    async func ComputeValueTask() -> Task<int> {
        return await ValueTask.FromResult(3)
    }
}
""";

        using var loaded = EmitAssembly(code, useRuntimeAsync: true);

        var programType = loaded.Assembly.GetType("Program", throwOnError: true)!;
        var methodFlags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static;
        var expectedAsyncMethods = new[]
        {
            "ComputeTaskConfigured",
            "ComputeTask",
            "ComputeValueTaskConfigured",
            "ComputeValueTask"
        };

        foreach (var methodName in expectedAsyncMethods)
        {
            var asyncMethod = programType.GetMethod(methodName, methodFlags)!;
            Assert.NotEqual(0, ((int)asyncMethod.GetMethodImplementationFlags()) & RuntimeAsyncMethodImplBit);
        }

        var generatedTypes = loaded.Assembly.GetTypes();
        Assert.DoesNotContain(
            generatedTypes,
            static t => t.Name.Contains("AsyncStateMachine", StringComparison.Ordinal));
    }

    [Fact]
    public void RuntimeAsyncEnabled_UsesRuntimeAwaitHelper_WhenAvailable_ElseFallsBackToAwaiterPattern()
    {
        const string code = """
import System.Threading.Tasks.*

class Program {
    async func Compute() -> Task<int> {
        return await Task.FromResult(1)
    }
}
""";

        using var loaded = EmitAssembly(code, useRuntimeAsync: true);

        var programType = loaded.Assembly.GetType("Program", throwOnError: true)!;
        var methodFlags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static;
        var asyncMethod = programType.GetMethod("Compute", methodFlags)!;
        var calledMembers = ILReader.GetCalledMembers(asyncMethod);

        var runtimeHasAsyncHelpers = typeof(System.Runtime.CompilerServices.AsyncTaskMethodBuilder)
            .Assembly
            .GetType("System.Runtime.CompilerServices.AsyncHelpers", throwOnError: false) is not null;

        if (runtimeHasAsyncHelpers)
        {
            Assert.Contains(
                calledMembers,
                static member => member.Contains("System.Runtime.CompilerServices.AsyncHelpers::Await", StringComparison.Ordinal));
            return;
        }

        Assert.Contains(
            calledMembers,
            static member => member.EndsWith("::GetAwaiter", StringComparison.Ordinal));
        Assert.Contains(
            calledMembers,
            static member => member.EndsWith("::GetResult", StringComparison.Ordinal));
    }

    [Fact]
    public void RuntimeAsyncEnabled_Net11AsyncTaskEntryPoint_UsesRuntimeEntryPointHandler_WhenAvailable()
    {
        if (!RuntimeAsyncEntryPointHandlerAvailable())
            return;

        const string code = """
import System.Threading.Tasks.*

async func Main() -> Task<int> {
    await Task.Yield()
    return 5
}
""";

        using var loaded = EmitAssembly(
            code,
            useRuntimeAsync: true,
            outputKind: OutputKind.ConsoleApplication,
            references: GetFrameworkReferences("net11.0"));

        var entryPoint = loaded.Assembly.EntryPoint;
        Assert.NotNull(entryPoint);

        var calledMembers = ILReader.GetCalledMembers(entryPoint!);

        Assert.Contains(
            calledMembers,
            static member => member.Contains("System.Runtime.CompilerServices.AsyncHelpers::HandleAsyncEntryPoint", StringComparison.Ordinal));
        Assert.DoesNotContain(
            calledMembers,
            static member => member.EndsWith("::GetAwaiter", StringComparison.Ordinal));
        Assert.DoesNotContain(
            calledMembers,
            static member => member.EndsWith("::GetResult", StringComparison.Ordinal));
    }

    [Fact]
    public void RuntimeAsyncEnabled_TryCatchReturn_UsesEffectiveReturnTypeForExitLocal()
    {
        const string code = """
import System.*
import System.IO.*
import System.Threading.Tasks.*

class Program {
    async func Fetch() -> Task<int> {
        use stream = MemoryStream()
        try {
            val value = await Task.FromResult(42)
            return value
        } catch (Exception e) {
            return -1
        }
    }
}
""";

        using var loaded = EmitAssembly(code, useRuntimeAsync: true);

        var programType = loaded.Assembly.GetType("Program", throwOnError: true)!;
        var methodFlags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static;
        var fetchMethod = programType.GetMethod("Fetch", methodFlags)!;
        var methodBody = fetchMethod.GetMethodBody();
        Assert.NotNull(methodBody);

        Assert.DoesNotContain(
            methodBody!.LocalVariables,
            local => local.LocalType == fetchMethod.ReturnType);

    }

    [Fact]
    public void RuntimeAsyncEnabled_BlockBodiedAsyncLambda_ReturnsExpectedTaskResult()
    {
        const string code = """
import System.Threading.Tasks.*

class Program {
    public async func Compute() -> Task<string> {
        val run = async () => {
            await Task.Delay(1)
            return "ok"
        }

        return await run()
    }
}
""";

        using var loaded = EmitAssembly(code, useRuntimeAsync: true);

        var programType = loaded.Assembly.GetType("Program", throwOnError: true)!;
        var methodFlags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static;
        var computeMethod = programType.GetMethod("Compute", methodFlags)!;
        var computeCalls = ILReader.GetCalledMembers(computeMethod);
        Assert.DoesNotContain(
            computeCalls,
            static member => member.Contains("System.Threading.Tasks.Task::FromResult", StringComparison.Ordinal));

        var lambdaMethod = loaded.Assembly
            .GetTypes()
            .SelectMany(type => type.GetMethods(methodFlags))
            .FirstOrDefault(static method =>
                method.Name.Contains("<lambda_", StringComparison.Ordinal) ||
                method.Name.Contains("<Compute>b__", StringComparison.Ordinal));
        Assert.NotNull(lambdaMethod);

        var lambdaCalls = ILReader.GetCalledMembers(lambdaMethod!);
        var runtimeHasAsyncHelpers = typeof(System.Runtime.CompilerServices.AsyncTaskMethodBuilder)
            .Assembly
            .GetType("System.Runtime.CompilerServices.AsyncHelpers", throwOnError: false) is not null;

        if (runtimeHasAsyncHelpers)
        {
            Assert.Contains(
                lambdaCalls,
                static member => member.Contains("System.Runtime.CompilerServices.AsyncHelpers::Await", StringComparison.Ordinal));
        }
        else
        {
            Assert.Contains(
                lambdaCalls,
                static member => member.EndsWith("::GetAwaiter", StringComparison.Ordinal));
            Assert.Contains(
                lambdaCalls,
                static member => member.EndsWith("::GetResult", StringComparison.Ordinal));
        }

        Assert.DoesNotContain(
            lambdaCalls,
            static member => member.Contains("System.Threading.Tasks.Task::FromResult", StringComparison.Ordinal));
    }

    private static TestAssemblyLoader.LoadedAssembly EmitAssembly(
        string code,
        bool useRuntimeAsync,
        OutputKind outputKind = OutputKind.DynamicallyLinkedLibrary,
        MetadataReference[]? references = null)
    {
        var syntaxTree = SyntaxTree.ParseText(code);
        references ??= TestMetadataReferences.Default;

        var compilation = Compilation.Create(
            $"runtime-async-{Guid.NewGuid():N}",
            [syntaxTree],
            references,
            new CompilationOptions(outputKind).WithRuntimeAsync(useRuntimeAsync));

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        return TestAssemblyLoader.LoadFromStream(peStream, references);
    }

    private static MetadataReference[] GetFrameworkReferences(string targetFramework)
    {
        var version = TargetFrameworkResolver.ResolveVersion(targetFramework);
        return TargetFrameworkResolver.GetReferenceAssemblies(version)
            .Where(File.Exists)
            .Select(MetadataReference.CreateFromFile)
            .ToArray();
    }

    private static bool RuntimeAsyncEntryPointHandlerAvailable()
    {
        var asyncHelpersType = typeof(System.Runtime.CompilerServices.AsyncTaskMethodBuilder)
            .Assembly
            .GetType("System.Runtime.CompilerServices.AsyncHelpers", throwOnError: false);

        return asyncHelpersType?
            .GetMethods(BindingFlags.Public | BindingFlags.Static)
            .Any(static method => string.Equals(method.Name, "HandleAsyncEntryPoint", StringComparison.Ordinal)) == true;
    }
}
