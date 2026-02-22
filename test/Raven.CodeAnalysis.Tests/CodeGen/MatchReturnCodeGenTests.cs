using System;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Threading.Tasks;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class MatchReturnCodeGenTests
{
    [Fact]
    public void MatchReturningBool_DoesNotEmitInvalidUnbox()
    {
        var code = """
union Flag {
    On
    Off
}

class Program {
    public static IsOn(flag: Flag) -> bool {
        return flag match {
            .On => true
            .Off => false
        }
    }

    public static TestOn() -> bool {
        return IsOn(.On)
    }

    public static TestOff() -> bool {
        return IsOn(.Off)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion("net9.0");
        MetadataReference[] references = [
            .. TargetFrameworkResolver.GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var programType = runtimeAssembly.GetType("Program", throwOnError: true)!;
        var testOn = programType.GetMethod("TestOn", BindingFlags.Public | BindingFlags.Static)!;
        var testOff = programType.GetMethod("TestOff", BindingFlags.Public | BindingFlags.Static)!;

        Assert.True((bool)testOn.Invoke(null, null)!);
        Assert.False((bool)testOff.Invoke(null, null)!);
    }

    // Generic union constructor type inference: Ok(42) should infer Ok<int>
    // when Ok is a case of a generic union and T is inferrable from the argument.
    [Fact]
    public void GenericUnionCase_ImplicitReturn_InfersTypeArgFromArgument()
    {
        // Top-level code must appear before declarations; function/union declarations follow.
        var code = """
import System.*
import System.Console.*

val okResult = GetOk()
val errResult = GetErr()

WriteLine(okResult match {
    .Ok(val v) => "ok:${v}"
    .Err(val e) => "error:${e}"
})

WriteLine(errResult match {
    .Ok(val v) => "ok:${v}"
    .Err(val e) => "error:${e}"
})

func GetOk() -> Outcome<int, string> {
    Ok(42)
}

func GetErr() -> Outcome<int, string> {
    Err("fail")
}

union Outcome<T, E> {
    Ok(value: T)
    Err(data: E)
}
""";

        var output = RunAndCaptureOutput(code);
        Assert.Equal("ok:42\nerror:fail", output);
    }

    // If/else in last position of a value-returning function body should be
    // promoted to an implicit return, not produce RAV2108.
    [Fact]
    public void IfElse_AsImplicitReturn_ReturnsCorrectBranch()
    {
        var code = """
import System.*
import System.Console.*

val okResult = Compute(true)
val errResult = Compute(false)

WriteLine(okResult match {
    .Ok(val v) => "ok:${v}"
    .Err(val e) => "error:${e}"
})

WriteLine(errResult match {
    .Ok(val v) => "ok:${v}"
    .Err(val e) => "error:${e}"
})

func Compute(flag: bool) -> Outcome<int, string> {
    if flag {
        Ok(99)
    } else {
        Err("nope")
    }
}

union Outcome<T, E> {
    Ok(value: T)
    Err(data: E)
}
""";

        var output = RunAndCaptureOutput(code);
        Assert.Equal("ok:99\nerror:nope", output);
    }

    // The same if/else implicit return scenario inside an async method.
    [Fact]
    public void AsyncIfElse_AsImplicitReturn_ReturnsCorrectBranch()
    {
        var code = """
import System.*
import System.Console.*
import System.Threading.Tasks.*

val okResult = await Compute(true)
val errResult = await Compute(false)

WriteLine(okResult match {
    .Ok(val v) => "ok:${v}"
    .Err(val e) => "error:${e}"
})

WriteLine(errResult match {
    .Ok(val v) => "ok:${v}"
    .Err(val e) => "error:${e}"
})

async func Compute(flag: bool) -> Task<Outcome<int, string>> {
    await Task.Delay(1)
    if flag {
        Ok(99)
    } else {
        Err("nope")
    }
}

union Outcome<T, E> {
    Ok(value: T)
    Err(data: E)
}
""";

        var output = RunAndCaptureOutput(code);
        Assert.Equal("ok:99\nerror:nope", output);
    }

    private static string RunAndCaptureOutput(string code)
    {
        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var entryPoint = loaded.Assembly.EntryPoint!;

        using var writer = new StringWriter();
        var originalOut = Console.Out;
        Console.SetOut(writer);
        try
        {
            var args = entryPoint.GetParameters().Length == 0
                ? null
                : new object?[] { Array.Empty<string>() };
            entryPoint.Invoke(null, args);
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        return writer.ToString().ReplaceLineEndings("\n").TrimEnd('\n');
    }
}
