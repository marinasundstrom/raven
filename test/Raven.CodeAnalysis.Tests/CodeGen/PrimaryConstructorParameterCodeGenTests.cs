using System;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

using Raven.CodeAnalysis.Semantics.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public sealed class PrimaryConstructorParameterCodeGenTests : CompilationTestBase
{
    [Fact]
    public void Emit_AsyncPrimaryConstructorClassMethod_ResolvesBodyParameterBuilder()
    {
        const string source = """
import System.*
import System.Threading.Tasks.*
import System.Threading.Tasks.Task.*

union Result<T, E> {
    case Ok(value: T)
    case Error(value: E)
}

class ShipmentOrderService(private var pendingCount: int = 0) {
    async func BuildQuote(weightKg: int) -> Task<Result<int, string>> {
        await Delay(20)

        if weightKg > 0 {
            return Ok(weightKg * 2)
        }

        return Error("Weight must be positive")
    }
}
""";

        var (compilation, _) = CreateCompilation(source);
        compilation.EnsureSetup();

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(
            result.Success,
            string.Join(Environment.NewLine, result.Diagnostics.Select(diagnostic => diagnostic.ToString())));

        peStream.Position = 0;
        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var type = loaded.Assembly.GetType("ShipmentOrderService", throwOnError: true)!;
        var instance = Activator.CreateInstance(type, new object[] { 0 })!;
        var method = type.GetMethod("BuildQuote", new[] { typeof(int) })!;

        var task = Assert.IsAssignableFrom<Task>(method.Invoke(instance, new object[] { 21 }));
        task.GetAwaiter().GetResult();
    }
}
