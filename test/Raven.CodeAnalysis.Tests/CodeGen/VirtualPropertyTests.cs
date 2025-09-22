using System;
using System.IO;
using System.Reflection;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class VirtualPropertyTests
{
    [Fact]
    public void VirtualOverrideProperty_DispatchesThroughBaseReference()
    {
        const string code = """
open class Animal {
    public virtual Name: string {
        get => "base"
    }
}

class Dog : Animal {
    public override Name: string {
        get => "dog"
    }
}

class Program {
    Run() -> string {
        let animal: Animal = Dog()
        return animal.Name
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
        var runMethod = type.GetMethod("Run", BindingFlags.Instance | BindingFlags.Public)!;

        var value = (string)runMethod.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal("dog", value);
    }
}
