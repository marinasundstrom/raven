using System;
using System.IO;
using System.Reflection;

using Raven;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests.Utilities;

using Xunit.Abstractions;

namespace Raven.CodeAnalysis.Tests;

public sealed class AsyncPatternReceiverCodeGenTests(ITestOutputHelper output)
{
    private readonly ITestOutputHelper _output = output;

    [Fact]
    public void InstanceMemberInComparisonPattern_AfterAwait_ExecutesAndPassesIlVerify()
    {
        const string code = """
import System.Threading.Tasks.*

class ThresholdMatcher {
    val Threshold = 10

    public async func Matches(value: int) -> Task<bool> {
        await Task.Delay(1)
        return value is > Threshold
    }
}
""";

        var framework = TargetFrameworkResolver.ResolveVersion("net10.0");
        var references = TargetFrameworkResolver
            .GetReferenceAssemblies(framework)
            .Select(MetadataReference.CreateFromFile)
            .ToArray();
        var compilation = Compilation.Create(
            "async-pattern-receiver",
            [SyntaxTree.ParseText(code)],
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

            if (IlVerifyTestHelper.TryResolve(_output))
            {
                Assert.True(
                    IlVerifyRunner.Verify(null, assemblyPath, compilation),
                    "IL verification failed for an instance member used by a comparison pattern after await.");
            }

            using var assemblyStream = File.OpenRead(assemblyPath);
            using var loaded = TestAssemblyLoader.LoadFromStream(assemblyStream, references);
            var matcherType = loaded.Assembly.GetType("ThresholdMatcher", throwOnError: true)!;
            var matcher = Activator.CreateInstance(matcherType);
            var method = matcherType.GetMethod("Matches", BindingFlags.Public | BindingFlags.Instance)!;

            var above = Assert.IsAssignableFrom<Task<bool>>(method.Invoke(matcher, [11]));
            var below = Assert.IsAssignableFrom<Task<bool>>(method.Invoke(matcher, [9]));
            Assert.True(above.GetAwaiter().GetResult());
            Assert.False(below.GetAwaiter().GetResult());
        }
        finally
        {
            if (File.Exists(assemblyPath))
                File.Delete(assemblyPath);
        }
    }
}
