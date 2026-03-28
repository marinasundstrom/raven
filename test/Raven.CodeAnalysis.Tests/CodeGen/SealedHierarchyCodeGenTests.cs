using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class SealedHierarchyCodeGenTests
{
    [Fact]
    public void GenericSealedInterface_NestedCases_ExecuteWithoutBadImageFormat()
    {
        const string code = """
import System.*
import System.Console.*

sealed interface Expr<T> {
    record NumericalExpr(Value: float) : Expr<float>
    record StringExpr(Value: string) : Expr<string>
    record AddExpr(Left: Expr<float>, Right: Expr<float>) : Expr<float>
}

class Program {
    public static func Run() -> string {
        val left = Expr.NumericalExpr(40)
        val right = Expr.NumericalExpr(2)
        val result = Expr.AddExpr(left, right)
        return result.ToString()
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
        var assembly = loaded.Assembly;
        var programType = assembly.GetType("Program", throwOnError: true)!;
        var run = programType.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;

        var value = (string)run.Invoke(null, Array.Empty<object>())!;
        Assert.Contains("AddExpr", value, StringComparison.Ordinal);
    }
}
