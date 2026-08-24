using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public sealed class GenericMathEmissionTests
{
    [Fact]
    public void ConstrainedGenericArithmetic_ExecutesAfterReload()
    {
        const string code = """
import System.Numerics.*

class GenericMathRunner {
    static func Calculate<T>(left: T, right: T) -> T
        where T: INumber<T> {
        (left + right) * right
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "generic-math-emission",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        peStream.Position = 0;
        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var runner = loaded.Assembly.GetType("GenericMathRunner", throwOnError: true)!;
        var method = runner
            .GetMethod("Calculate", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static)!
            .MakeGenericMethod(typeof(int));

        Assert.Equal(24, method.Invoke(null, [2, 4]));
        Assert.Equal(24m, method.GetGenericMethodDefinition()
            .MakeGenericMethod(typeof(decimal))
            .Invoke(null, [2m, 4m]));
    }
}
