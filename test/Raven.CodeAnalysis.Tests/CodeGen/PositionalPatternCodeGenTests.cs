using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class PositionalPatternCodeGenTests
{
    [Fact(Skip = "Positional pattern codegen is currently unstable and tracked separately.")]
    public void MatchExpression_WithPositionalPattern_EmitsSuccessfully()
    {
        const string code = """
import System.*

func describe(value: object) -> string {
    value match {
        (val first: int, val second: int) => "${first + second}"
        _ => "no match"
    }
}

func Main() {
    val value: object = (1, 2)
    Console.WriteLine(describe(value))
}
""";

        var tree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("tuple_pattern_emit", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(references);

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        var diagnosticMessage = string.Join(Environment.NewLine, result.Diagnostics.Select(d => d.ToString()));
        Assert.True(result.Success, diagnosticMessage);
    }

    [Fact(Skip = "Positional pattern codegen is currently unstable and tracked separately.")]
    public void LetPositionalPatternAssignment_EmitsSuccessfully()
    {
        const string code = """
import System.*

func Main() {
    (val first, val second) = (1, 2)
    Console.WriteLine(first)
    Console.WriteLine(second)
}
""";

        var tree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("let_tuple_pattern_emit", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(references);

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        var diagnosticMessage = string.Join(Environment.NewLine, result.Diagnostics.Select(d => d.ToString()));
        Assert.True(result.Success, diagnosticMessage);
    }

    [Fact(Skip = "Positional pattern codegen is currently unstable and tracked separately.")]
    public void PositionalPatternAssignment_WithExistingLocals_EmitsSuccessfully()
    {
        const string code = """
import System.*

func Main() {
    var first = 0
    var second = 0
    (first, second) = (1, 2)
    Console.WriteLine(first + second)
}
""";

        var tree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("tuple_pattern_existing_locals", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(references);

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        var diagnosticMessage = string.Join(Environment.NewLine, result.Diagnostics.Select(d => d.ToString()));
        Assert.True(result.Success, diagnosticMessage);
    }

    [Fact(Skip = "Positional pattern codegen is currently unstable and tracked separately.")]
    public void VarPositionalPatternAssignment_EmitsSuccessfully()
    {
        const string code = """
import System.*

func Main() {
    (var first, var second) = (1, 2)
    Console.WriteLine(first + second)
}
""";

        var tree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("tuple_pattern_var_assignment", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(references);

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        var diagnosticMessage = string.Join(Environment.NewLine, result.Diagnostics.Select(d => d.ToString()));
        Assert.True(result.Success, diagnosticMessage);
    }

    [Fact(Skip = "Positional pattern codegen is currently unstable and tracked separately.")]
    public void MixedPositionalPatternAssignment_EmitsSuccessfully()
    {
        const string code = """
import System.*

func Main() {
    (val first, val ignoredFirst) = (1, 2)
    (var second, var ignoredSecond) = (3, 4)
    Console.WriteLine(first)
    Console.WriteLine(second)
}
""";

        var tree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("tuple_pattern_mixed_assignment", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(references);

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        var diagnosticMessage = string.Join(Environment.NewLine, result.Diagnostics.Select(d => d.ToString()));
        Assert.True(result.Success, diagnosticMessage);
    }
}
