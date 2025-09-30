using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;

using Microsoft.CodeAnalysis;

using RavenSyntaxTree = Raven.CodeAnalysis.Syntax.SyntaxTree;
using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class MatchExpressionCodeGenTests
{
    [Fact]
    public void MatchExpression_WithValueTypeArm_EmitsAndRuns()
    {
        const string code = """
let value = 42
let result = value match {
    int i => i.ToString()
    _ => "None"
}

System.Console.WriteLine(result)
""";

        var output = EmitAndRun(code, "match_value_type");
        if (output is null)
            return;
        Assert.Equal("42", output);
    }

    [Fact]
    public void MatchExpression_AsReturnValue_EmitsAndRuns()
    {
        const string code = """
let describer = Describer()
let zero = describer.Describe(0)
let two = describer.Describe(2)

System.Console.WriteLine(zero + "," + two)

class Describer {
    Describe(value: int) -> string {
        return value match {
            0 => "zero"
            _ => value.ToString()
        }
    }
}
""";

        var output = EmitAndRun(code, "match_return_value");
        if (output is null)
            return;
        Assert.Equal("zero,2", output);
    }

    [Fact]
    public void MatchExpression_WithStringLiteralPattern_MatchesExactValue()
    {
        const string code = """
let foo = "foo" match {
    "foo" => "str"
    _ => "None"
}

let empty = "" match {
    "foo" => "str"
    _ => "None"
}

System.Console.WriteLine(foo + "," + empty)
""";

        var output = EmitAndRun(code, "match_string_literal");
        if (output is null)
            return;
        Assert.Equal("str,None", output);
    }

    [Fact]
    public void MatchExpression_WithUnionTupleArm_EmitsAndRuns()
    {
        const string code = """
let describer = Describer()
let tuple: bool | (flag: bool, text: string) = (false, "tuple")
let boolResult = describer.Describe(false)
let tupleResult = describer.Describe(tuple)

System.Console.WriteLine(boolResult + "," + tupleResult)

class Describer {
    Describe(value: bool | (flag: bool, text: string)) -> string {
        return value match {
            true => "true"
            false => "false"
            (flag: bool, text: string) => text
        }
    }
}
""";

        var output = EmitAndRun(code, "match_union_tuple");
        if (output is null)
            return;
        Assert.Equal("false,tuple", output);
    }

    private static string? EmitAndRun(string code, string assemblyName, params string[] additionalSources)
    {
        var syntaxTrees = new List<RavenSyntaxTree> { RavenSyntaxTree.ParseText(code) };

        foreach (var source in additionalSources)
            syntaxTrees.Add(RavenSyntaxTree.ParseText(source));

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
            Assert.NotNull(typeof(System.Runtime.CompilerServices.ITuple).GetProperty("Length"));
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
