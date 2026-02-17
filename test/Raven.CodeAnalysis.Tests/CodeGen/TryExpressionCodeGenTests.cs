using System;
using System.IO;

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
    public void TryExpression_WithResultOperand_PassesIlVerifyWhenToolAvailable()
    {
        if (!IlVerifyTestHelper.TryResolve(_output))
        {
            _output.WriteLine("Skipping IL verification because ilverify was not found.");
            return;
        }

        const string code = """
import System.*
import System.Console.*
import System.Collections.Generic.*

val arr = [1, 2, 3]

val x = try arr.FirstOrError(() => DomainError.NotFound)

WriteLine(x)

union DomainError {
    NotFound
    Unexpected(exception: Exception)
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = GetReferencesWithRavenCore();
        var compilation = Compilation.Create(
            "try-expression-result-operand",
            [syntaxTree],
            references,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var assemblyPath = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid():N}.dll");

        try
        {
            using (var peStream = File.Create(assemblyPath))
            {
                var emitResult = compilation.Emit(peStream);
                Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));
            }

            var succeeded = IlVerifyRunner.Verify(null, assemblyPath, compilation);
            Assert.True(succeeded, "IL verification failed for try-expression over Result operand.");
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
