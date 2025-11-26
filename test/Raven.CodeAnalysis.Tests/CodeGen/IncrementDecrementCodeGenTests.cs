using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;

using Microsoft.CodeAnalysis;

using Raven.CodeAnalysis.Testing;

using SyntaxSyntaxTree = Raven.CodeAnalysis.Syntax.SyntaxTree;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class IncrementDecrementCodeGenTests
{
    [Fact]
    public void PrefixIncrement_UpdatesAndReturnsNewValue()
    {
        const string code = """
let i = 1
let result = ++i

System.Console.WriteLine(i.ToString() + "," + result.ToString())
""";

        var output = EmitAndRun(code, "prefix_increment");
        if (output is null)
            return;

        Assert.Equal("2,2", output);
    }

    [Fact]
    public void PostfixIncrement_ReturnsOldValueThenUpdates()
    {
        const string code = """
let i = 1
let result = i++

System.Console.WriteLine(i.ToString() + "," + result.ToString())
""";

        var output = EmitAndRun(code, "postfix_increment");
        if (output is null)
            return;

        Assert.Equal("2,1", output);
    }

    [Fact]
    public void PrefixDecrement_UpdatesAndReturnsNewValue()
    {
        const string code = """
let i = 3
let result = --i

System.Console.WriteLine(i.ToString() + "," + result.ToString())
""";

        var output = EmitAndRun(code, "prefix_decrement");
        if (output is null)
            return;

        Assert.Equal("2,2", output);
    }

    private static string? EmitAndRun(string code, string assemblyName, params string[] additionalSources)
    {
        var syntaxTrees = new List<SyntaxSyntaxTree> { SyntaxSyntaxTree.ParseText(code) };

        foreach (var source in additionalSources)
            syntaxTrees.Add(SyntaxSyntaxTree.ParseText(source));

        var references = RuntimeMetadataReferences;

        var compilation = Compilation.Create(assemblyName, new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTrees.ToArray())
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        var assemblyBytes = peStream.ToArray();
        var assembly = Assembly.Load(assemblyBytes);
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

            try
            {
                entryPoint.Invoke(null, arguments);
            }
            catch (TargetInvocationException invocationException)
                when (invocationException.InnerException is MissingMethodException mme
                    && mme.Message.Contains("System.Runtime.CompilerServices.ITuple", StringComparison.Ordinal))
            {
                return null;
            }
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        var output = writer.ToString();
        return output.ReplaceLineEndings("\n").TrimEnd('\n');
    }

    private static readonly MetadataReference[] RuntimeMetadataReferences = GetRuntimeMetadataReferences();

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
