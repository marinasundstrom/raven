using System;
using System.IO;
using System.Linq;
using System.Reflection;

using Microsoft.CodeAnalysis;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

using RavenSyntaxTree = Raven.CodeAnalysis.Syntax.SyntaxTree;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public sealed class MatchStatementCodeGenTests
{
    [Fact]
    public void MatchStatement_WithUnitArms_AtTopLevel_EmitsAndRuns()
    {
        const string code = """
val condition = true

match condition {
    true => System.Console.WriteLine("ok")
    false => System.Console.WriteLine("nope")
}
""";

        var output = EmitAndRun(code, "match_statement_unit_top_level");
        Assert.Equal("ok", output);
    }

    private static string EmitAndRun(string code, string assemblyName)
    {
        var syntaxTree = RavenSyntaxTree.ParseText(code);

        var compilation = Compilation.Create(assemblyName, new CompilationOptions(OutputKind.ConsoleApplication))
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
                1 => [Array.Empty<string>()],
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
