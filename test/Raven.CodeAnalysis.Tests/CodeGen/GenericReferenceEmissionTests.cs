using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests.Utilities;

using Xunit.Abstractions;

namespace Raven.CodeAnalysis.Tests;

public sealed class GenericReferenceEmissionTests(ITestOutputHelper output)
{
    private readonly ITestOutputHelper _output = output;

    [Fact]
    public void NullableReferenceTypeParameterPattern_ExecutesAndPassesIlVerifyWhenToolAvailable()
    {
        const string code = """
class GenericReferenceRunner {
    static func PresentOrNull<T : class>(value: T?) -> T? {
        if let present: T = value {
            return present
        }

        return null
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "generic-reference-emission",
            [syntaxTree],
            TestMetadataReferences.Default,
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
                    "IL verification failed for a nullable reference-constrained generic pattern.");
            }

            using var assemblyStream = File.OpenRead(assemblyPath);
            using var loaded = TestAssemblyLoader.LoadFromStream(assemblyStream, TestMetadataReferences.Default);
            var runner = loaded.Assembly.GetType("GenericReferenceRunner", throwOnError: true)!;
            var method = runner
                .GetMethod("PresentOrNull", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static)!
                .MakeGenericMethod(typeof(string));

            Assert.Equal("raven", method.Invoke(null, ["raven"]));
            Assert.Null(method.Invoke(null, [null]));
        }
        finally
        {
            if (File.Exists(assemblyPath))
                File.Delete(assemblyPath);
        }
    }
}
