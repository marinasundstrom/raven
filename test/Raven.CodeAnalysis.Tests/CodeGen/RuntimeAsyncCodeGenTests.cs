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
    async Compute() -> Task<int> {
        return await Task.FromResult(1)
    }

    Sync() -> int {
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
    async Compute() -> Task<int> {
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
    async Compute() -> Task<int> {
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
    async ComputeTaskConfigured() -> Task<int> {
        return await Task.FromResult(1).ConfigureAwait(false)
    }

    async ComputeTask() -> Task {
        await Task.Delay(1).ConfigureAwait(false)
    }

    async ComputeValueTaskConfigured() -> Task<int> {
        return await ValueTask.FromResult(2).ConfigureAwait(false)
    }

    async ComputeValueTask() -> Task<int> {
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
    async Compute() -> Task<int> {
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
    public void RuntimeAsyncEnabled_TryCatchReturn_UsesEffectiveReturnTypeForExitLocal()
    {
        const string code = """
import System.*
import System.IO.*
import System.Threading.Tasks.*

class Program {
    async Fetch() -> Task<int> {
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

    private static TestAssemblyLoader.LoadedAssembly EmitAssembly(string code, bool useRuntimeAsync)
    {
        var syntaxTree = SyntaxTree.ParseText(code);

        var compilation = Compilation.Create(
            $"runtime-async-{Guid.NewGuid():N}",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithRuntimeAsync(useRuntimeAsync));

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        return TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
    }
}
