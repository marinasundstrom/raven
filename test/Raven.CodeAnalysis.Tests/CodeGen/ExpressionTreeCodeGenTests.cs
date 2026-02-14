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
    Eval(value: int) -> bool {
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
    Build() -> Expression<System.Func<int, bool>> {
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
}
