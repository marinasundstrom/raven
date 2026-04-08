using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Threading;
using System.Threading.Tasks;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.CodeGen.Async;

public sealed class AsyncIteratorCancellationCodeGenTests
{
    [Fact]
    public async Task AsyncIterator_EnumeratorCancellationAttribute_UsesGetAsyncEnumeratorToken()
    {
        const string code = """
import System.Collections.Generic.*
import System.Runtime.CompilerServices.*
import System.Threading.*

class Counter {
    async func Values([EnumeratorCancellation] cancellationToken: CancellationToken) -> IAsyncEnumerable<int> {
        yield return 1
        if cancellationToken.IsCancellationRequested {
            yield break
        }

        yield return 2
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
        var type = assembly.GetType("Counter", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Values", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)!;

        var values = (IAsyncEnumerable<int>)method.Invoke(instance, new object[] { default(CancellationToken) })!;
        using var cts = new CancellationTokenSource();
        await using var enumerator = values.GetAsyncEnumerator(cts.Token);

        Assert.True(await enumerator.MoveNextAsync());
        Assert.Equal(1, enumerator.Current);

        cts.Cancel();

        Assert.False(await enumerator.MoveNextAsync());
    }

    [Fact]
    public async Task AsyncIterator_EnumeratorCancellationAttribute_PreservesDirectMethodToken()
    {
        const string code = """
import System.Collections.Generic.*
import System.Runtime.CompilerServices.*
import System.Threading.*
import System.Threading.Tasks.*

class Counter {
    async func Values([EnumeratorCancellation] cancellationToken: CancellationToken) -> IAsyncEnumerable<int> {
        yield return 1
        await Task.Delay(5000, cancellationToken)
        yield return 2
    }
}
""";

        var method = CompileValuesMethod(code);
        var instance = Activator.CreateInstance(method.DeclaringType!)!;

        using var cts = new CancellationTokenSource();
        var values = (IAsyncEnumerable<int>)method.Invoke(instance, new object[] { cts.Token })!;
        await using var enumerator = values.GetAsyncEnumerator();

        Assert.True(await enumerator.MoveNextAsync());
        Assert.Equal(1, enumerator.Current);

        cts.Cancel();

        await Assert.ThrowsAnyAsync<OperationCanceledException>(async () => _ = await enumerator.MoveNextAsync());
    }

    [Fact]
    public async Task AsyncIterator_EnumeratorCancellationAttribute_LinksMethodAndEnumeratorTokens()
    {
        const string code = """
import System.Collections.Generic.*
import System.Runtime.CompilerServices.*
import System.Threading.*
import System.Threading.Tasks.*

class Counter {
    async func Values([EnumeratorCancellation] cancellationToken: CancellationToken) -> IAsyncEnumerable<int> {
        yield return 1
        await Task.Delay(5000, cancellationToken)
        yield return 2
    }
}
""";

        var method = CompileValuesMethod(code);
        var instance = Activator.CreateInstance(method.DeclaringType!)!;

        using var methodCts = new CancellationTokenSource();
        var values = (IAsyncEnumerable<int>)method.Invoke(instance, new object[] { methodCts.Token })!;

        using var enumeratorCts = new CancellationTokenSource();
        await using var enumerator = values.GetAsyncEnumerator(enumeratorCts.Token);

        Assert.True(await enumerator.MoveNextAsync());
        Assert.Equal(1, enumerator.Current);

        enumeratorCts.Cancel();

        await Assert.ThrowsAnyAsync<OperationCanceledException>(async () => _ = await enumerator.MoveNextAsync());
    }

    private static MethodInfo CompileValuesMethod(string code)
    {
        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Counter", throwOnError: true)!;
        return type.GetMethod("Values", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)!;
    }
}
