using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests.Utilities;

using Xunit;
using Xunit.Abstractions;

namespace Raven.CodeAnalysis.Tests;

public sealed class TryExpressionCodeGenTests(ITestOutputHelper output)
{
    private readonly ITestOutputHelper _output = output;

    [Fact]
    public void TryPropagation_AdaptsThrowingApiOnSuccessAndFailure()
    {
        const string code = """
import System.*

class Runner {
    static func Import(text: string) -> Result<int, Exception> {
        val value = try? Convert.ToInt32(text)
        return .Ok(value)
    }

    static func Run(text: string) -> string {
        return Import(text) match {
            .Ok(let value) => "value: $value"
            .Error(let error) => "error: ${error.Message}"
        }
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = GetReferencesWithRavenCore();
        var compilation = Compilation.Create(
            "try-propagation-throwing-api",
            [syntaxTree],
            references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        peStream.Position = 0;
        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var run = loaded.Assembly.GetType("Runner")!.GetMethod(
            "Run",
            BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static)!;

        Assert.Equal("value: 42", run.Invoke(null, ["42"]));
        Assert.StartsWith("error: ", Assert.IsType<string>(run.Invoke(null, ["expired"])));
    }

    [Fact]
    public void TryPropagation_WithResultOperand_PassesIlVerifyWhenToolAvailable()
    {
        if (!IlVerifyTestHelper.TryResolve(_output))
        {
            _output.WriteLine("Skipping IL verification because ilverify was not found.");
            return;
        }

        const string code = """
import System.*
import System.Threading.Tasks.*

class Runner {
    static async func Test(throwExc: bool) -> Task<Result<int, Exception>> {
        val x = try? await Action2(throwExc)
        return .Ok(x + 2)
    }

    static async func Action2(throwExc: bool) -> Task<Result<int, Exception>> {
        await Task.Delay(1)
        if throwExc {
            throw new Exception("Boom!")
        }

        return .Ok(40)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = GetReferencesWithRavenCore();
        var compilation = Compilation.Create(
            "try-propagation-result-operand",
            [syntaxTree],
            references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var assemblyPath = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid():N}.dll");

        try
        {
            using (var peStream = File.Create(assemblyPath))
            {
                var emitResult = compilation.Emit(peStream);
                Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));
            }

            var succeeded = IlVerifyRunner.Verify(null, assemblyPath, compilation);
            Assert.True(succeeded, "IL verification failed for try?-expression over Result operand.");
        }
        finally
        {
            if (File.Exists(assemblyPath))
                File.Delete(assemblyPath);
        }
    }

    private static MetadataReference[] GetReferencesWithRavenCore()
    {
        var corePath = Path.Combine(AppContext.BaseDirectory, "Raven.Core.dll");
        if (!File.Exists(corePath))
            return TestMetadataReferences.Default;

        return [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(corePath)];
    }
}
