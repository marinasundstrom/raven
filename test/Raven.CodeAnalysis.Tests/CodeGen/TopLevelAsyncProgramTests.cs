using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Threading.Tasks;

using Microsoft.CodeAnalysis;

using RavenSyntaxTree = Raven.CodeAnalysis.Syntax.SyntaxTree;
using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class TopLevelAsyncProgramTests
{
    [Fact]
    public void TopLevelGenericAsyncProgram_AwaitsValue()
    {
        const string code = """
import System.Console.*
import System.Threading.Tasks.*

async func Test<T>(value: T) -> Task<T> {
    await Task.Delay(10)
    return value
}

let result = await Test(42)

WriteLine(result)
""";

        var assembly = EmitAssembly(code, "top_level_async_generic");
        var programType = assembly.GetType("Program", throwOnError: true)!;
        var mainAsync = programType.GetMethod(
            "MainAsync",
            BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static);

        Assert.NotNull(mainAsync);

        var originalOut = Console.Out;
        using var writer = new StringWriter();
        Console.SetOut(writer);

        try
        {
            var task = mainAsync!.Invoke(null, new object?[] { Array.Empty<string>() });
            Assert.NotNull(task);

            switch (task)
            {
                case Task awaited:
                    awaited.GetAwaiter().GetResult();
                    break;
                default:
                    throw new InvalidOperationException($"Unexpected async return type '{task?.GetType()}'.");
            }
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        var output = writer.ToString().ReplaceLineEndings("\n").TrimEnd('\n');
        Assert.Equal("42", output);
    }

    private static Assembly EmitAssembly(string code, string assemblyName)
    {
        var syntaxTrees = new List<RavenSyntaxTree> { RavenSyntaxTree.ParseText(code) };
        var references = RuntimeMetadataReferences;

        var compilation = Compilation.Create(assemblyName, new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTrees.ToArray())
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        return Assembly.Load(peStream.ToArray());
    }

    private static MetadataReference[] RuntimeMetadataReferences => _runtimeMetadataReferences ??= GetRuntimeMetadataReferences();
    private static MetadataReference[]? _runtimeMetadataReferences;

    private static MetadataReference[] GetRuntimeMetadataReferences()
    {
        var tpa = AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES") as string;
        if (string.IsNullOrEmpty(tpa))
            return TestMetadataReferences.Default;

        var references = new List<MetadataReference>();
        var seen = new HashSet<string>(StringComparer.OrdinalIgnoreCase);

        foreach (var path in tpa.Split(Path.PathSeparator))
        {
            if (string.IsNullOrEmpty(path))
                continue;

            var name = Path.GetFileNameWithoutExtension(path);
            if (!seen.Add(name))
                continue;

            references.Add(MetadataReference.CreateFromFile(path));
        }

        return references.ToArray();
    }
}
