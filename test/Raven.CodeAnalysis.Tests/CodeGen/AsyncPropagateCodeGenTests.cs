using System;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Threading.Tasks;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class AsyncPropagateCodeGenTests
{
    [Fact]
    public async Task AsyncPropagate_UseDeclaration_DisposesOnSuccessAndFailure()
    {
        var code = """
import System.*
import System.Threading.Tasks.*

union Result<T, E> {
    Ok(value: T)
    Error(error: E)
}

class Disposable : IDisposable {
    public static var DisposedCount: int = 0

    public static Reset() {
        DisposedCount = 0
    }

    public Dispose() {
        DisposedCount = DisposedCount + 1
    }
}

class C {
    private static Fail() -> Result<int, string> {
        return .Error("boom")
    }

    private static Succeed() -> Result<int, string> {
        return .Ok(42)
    }

    public async RunFail() -> Task<Result<int, string>> {
        use d = Disposable()
        val value = try? await Task.FromResult(Fail())
        return .Ok(value)
    }

    public async RunSuccess() -> Task<Result<int, string>> {
        use d = Disposable()
        val value = try? await Task.FromResult(Succeed())
        return .Ok(value)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        MetadataReference[] references =
        [
            .. TargetFrameworkResolver
                .GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;

        var disposableType = assembly.GetType("Disposable", throwOnError: true)!;
        var reset = disposableType.GetMethod("Reset", BindingFlags.Public | BindingFlags.Static)!;
        var disposedCountProperty = disposableType.GetProperty("DisposedCount", BindingFlags.Public | BindingFlags.Static);
        var disposedCountField = disposableType.GetField("DisposedCount", BindingFlags.Public | BindingFlags.Static);

        var containerType = assembly.GetType("C", throwOnError: true)!;
        var instance = Activator.CreateInstance(containerType)!;
        var runFail = containerType.GetMethod("RunFail", BindingFlags.Public | BindingFlags.Instance)!;
        var runSuccess = containerType.GetMethod("RunSuccess", BindingFlags.Public | BindingFlags.Instance)!;

        reset.Invoke(null, null);
        var failResult = await InvokeTaskWithResultAsync(runFail, instance);
        Assert.Contains("Error", failResult.ToString(), StringComparison.Ordinal);
        Assert.Equal(1, ReadDisposedCount(disposedCountProperty, disposedCountField));

        reset.Invoke(null, null);
        var successResult = await InvokeTaskWithResultAsync(runSuccess, instance);
        Assert.Contains("Ok", successResult.ToString(), StringComparison.Ordinal);
        Assert.Equal(1, ReadDisposedCount(disposedCountProperty, disposedCountField));
    }

    private static async Task<object?> InvokeTaskWithResultAsync(MethodInfo method, object instance)
    {
        var task = (Task)method.Invoke(instance, null)!;
        await task.ConfigureAwait(false);
        return task.GetType().GetProperty("Result", BindingFlags.Public | BindingFlags.Instance)!.GetValue(task);
    }

    private static int ReadDisposedCount(PropertyInfo? property, FieldInfo? field)
    {
        if (property is not null)
            return (int)property.GetValue(null)!;
        if (field is not null)
            return (int)field.GetValue(null)!;

        throw new InvalidOperationException("Expected Disposable.DisposedCount to be emitted as a static property or field.");
    }
}
