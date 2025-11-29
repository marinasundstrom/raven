using System;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public sealed class AsyncLambdaStateMachineTests
{
    [Fact]
    public void TaskRun_async_lambda_executes_and_returns_value()
    {
        var output = EmitAndRun(
            """
            import System.Console.*
            import System.Threading.Tasks.*

            let value = await Task.Run(async () => {
                await Task.Delay(1)
                return 42
            })

            WriteLine(value)
            """
        );

        Assert.Equal("42", output);
    }

    [Fact]
    public void Nested_async_lambda_with_capture_executes_and_returns_value()
    {
        var output = EmitAndRun(
            """
            import System.Console.*
            import System.Threading.Tasks.*

            let offset = 2
            let run = async () => {
                let inner = async () => {
                    await Task.Delay(1)
                    return 40 + offset
                }

                return await inner()
            }

            let result = await Task.Run(run)
            WriteLine(result)
            """
        );

        Assert.Equal("42", output);
    }

    private static string EmitAndRun(string code)
    {
        var syntaxTree = SyntaxTree.ParseText(code);

        var compilation = Compilation.Create("async_lambdas", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(RuntimeMetadataReferences);

        compilation.EnsureSetup();

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        var assembly = Assembly.Load(peStream.ToArray());
        var entryPoint = assembly.EntryPoint;
        Assert.NotNull(entryPoint);

        var originalOut = Console.Out;
        using var writer = new StringWriter();
        Console.SetOut(writer);

        try
        {
            var parameters = entryPoint!.GetParameters();
            object?[]? arguments = parameters.Length switch
            {
                0 => null,
                1 => new object?[] { Array.Empty<string>() },
                _ => throw new InvalidOperationException("Unexpected entry point signature."),
            };

            entryPoint.Invoke(null, arguments);
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        return writer.ToString().ReplaceLineEndings("\n").TrimEnd('\n');
    }

    private static readonly MetadataReference[] RuntimeMetadataReferences = GetRuntimeMetadataReferences();

    private static MetadataReference[] GetRuntimeMetadataReferences()
    {
        var tpa = AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES") as string;
        if (string.IsNullOrEmpty(tpa))
            return TestMetadataReferences.Default;

        return tpa
            .Split(Path.PathSeparator)
            .Where(path => !string.IsNullOrEmpty(path))
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .Select(MetadataReference.CreateFromFile)
            .ToArray();
    }
}
