using System;
using System.IO;
using System.Linq;
using System.Reflection;

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
}
