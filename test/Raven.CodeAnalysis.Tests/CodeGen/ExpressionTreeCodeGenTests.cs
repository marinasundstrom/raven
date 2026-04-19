using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class ExpressionTreeCodeGenTests
{
    [Fact]
    public void ExpressionTree_LambdaCompile_Runs()
    {
        const string code = """
import System.*
import System.Linq.Expressions.*

class Checker {
    func Eval(value: int) -> bool {
        val tree: Expression<System.Func<int, bool>> = (x: int) => x > 0
        val compiled = tree.Compile()
        return compiled(value)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var checkerType = loaded.Assembly.GetType("Checker", throwOnError: true)!;
        var instance = Activator.CreateInstance(checkerType)!;
        var eval = checkerType.GetMethod("Eval", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)!;

        Assert.True((bool)eval.Invoke(instance, [5])!);
        Assert.False((bool)eval.Invoke(instance, [-1])!);
    }

    [Fact]
    public void ExpressionTree_OnlyLambda_DoesNotEmitLambdaMethod()
    {
        const string code = """
import System.*
import System.Linq.Expressions.*

class Checker {
    func Build() -> Expression<System.Func<int, bool>> {
        val tree: Expression<System.Func<int, bool>> = (x: int) => x > 0
        return tree
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var checkerType = loaded.Assembly.GetType("Checker", throwOnError: true)!;
        var methods = checkerType.GetMethods(BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic);

        Assert.DoesNotContain(methods, method => method.Name.Contains("<lambda_", StringComparison.Ordinal));
    }

    [Fact]
    public void ExpressionTree_LambdaInsideAsyncMethod_EmitsAndRuns()
    {
        const string code = """
import System.*
import System.Threading.Tasks.*
import System.Linq.Expressions.*

class QueryHost {
    static func Use(selector: Expression<System.Func<int, int>>) -> int {
        val compiled = selector.Compile()
        return compiled(41)
    }
}

class Demo {
    async func Run() -> Task<int> {
        val seed = await Task.FromResult(1)
        return QueryHost.Use(x => x + seed)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var demoType = loaded.Assembly.GetType("Demo", throwOnError: true)!;
        var instance = Activator.CreateInstance(demoType)!;
        var run = demoType.GetMethod("Run", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)!;
        var task = (Task<int>)run.Invoke(instance, Array.Empty<object>())!;

        Assert.Equal(42, task.GetAwaiter().GetResult());
    }

    [Fact]
    public void ExpressionTree_LambdaInsideAsyncLambdaCapturingLambdaParameter_EmitsAndRuns()
    {
        const string code = """
import System.*
import System.Threading.Tasks.*
import System.Linq.Expressions.*

class QueryHost {
    static async func Use(id: Guid, predicate: Expression<System.Func<Guid, bool>>) -> Task<int> {
        val compiled = predicate.Compile()
        if compiled(id) {
            return await Task.FromResult(1)
        }

        return await Task.FromResult(0)
    }
}

class Demo {
    static func Register(handler: System.Func<Guid, Task<int>>) -> System.Func<Guid, Task<int>> {
        return handler
    }

    static async func Run(id: Guid) -> Task<int> {
        val handler = Register(async func (captured: Guid) -> Task<int> {
            return await QueryHost.Use(captured, candidate => candidate == captured)
        })

        return await handler(id)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var demoType = loaded.Assembly.GetType("Demo", throwOnError: true)!;
        var run = demoType.GetMethod("Run", BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic)!;
        var id = Guid.NewGuid();
        var task = (Task<int>)run.Invoke(null, [id])!;

        Assert.Equal(1, task.GetAwaiter().GetResult());
    }
}
