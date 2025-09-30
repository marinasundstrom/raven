using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class MatchExpressionCodeGenTests
{
    [Fact]
    public void MatchExpression_WithValueTypeArm_EmitsAndRuns()
    {
        const string code = """
class Program {
    Run() -> string {
        let value = 42
        let result = value match {
            int i => i.ToString()
            _ => "None"
        }

        return result
    }

    Main() -> unit {
        return
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Program", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run")!;
        var value = (string)method.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal("42", value);
    }

    [Fact]
    public void MatchExpression_AsReturnValue_EmitsAndRuns()
    {
        const string code = """
class Program {
    Run(value: int) -> string {
        return value match {
            0 => "zero"
            _ => value.ToString()
        }
    }

    Main() -> unit {
        return
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Program", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run")!;
        var value = (string)method.Invoke(instance, new object[] { 2 })!;
        Assert.Equal("2", value);
    }

    [Fact]
    public void MatchExpression_WithStringLiteralPattern_MatchesExactValue()
    {
        const string code = """
class Program {
    Run() -> string {
        let foo = "foo" match {
            "foo" => "str"
            _ => "None"
        }

        let empty = "" match {
            "foo" => "str"
            _ => "None"
        }

        return foo + "," + empty
    }

    Main() -> unit {
        return
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Program", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run")!;
        var value = (string)method.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal("str,None", value);
    }

    [Fact]
    public void MatchExpression_WithUnionTupleArm_EmitsAndRuns()
    {
        const string code = """
class Program {
    Describe(value: bool | (flag: bool, text: string)) -> string {
        return value match {
            true => "true"
            false => "false"
            (flag: bool, text: string) => text
        }
    }

    Run() -> string {
        let tuple: bool | (flag: bool, text: string) = (false, "tuple")
        let boolResult = Describe(false)
        let tupleResult = Describe(tuple)
        return boolResult + "," + tupleResult
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("match_union_tuple", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Program", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run")!;
        var value = (string)method.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal("false,tuple", value);
    }
}
