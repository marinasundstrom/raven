using System;
using System.IO;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class EventTests
{
    [Fact]
    public void Event_AddRemoveInvoke_WiresHandlers()
    {
        var code = """
class Clicker {
    public event Clicked: System.Action;

    public Add(handler: System.Action) -> unit {
        Clicked += handler;
    }

    public Remove(handler: System.Action) -> unit {
        Clicked -= handler;
    }

    public Raise() -> unit {
        Clicked();
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
        var type = assembly.GetType("Clicker", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var addMethod = type.GetMethod("Add")!;
        var removeMethod = type.GetMethod("Remove")!;
        var raiseMethod = type.GetMethod("Raise")!;

        var hitA = 0;
        var hitB = 0;
        Action handlerA = () => hitA++;
        Action handlerB = () => hitB++;

        addMethod.Invoke(instance, new object[] { handlerA });
        addMethod.Invoke(instance, new object[] { handlerB });
        removeMethod.Invoke(instance, new object[] { handlerA });
        raiseMethod.Invoke(instance, Array.Empty<object>());

        Assert.Equal(0, hitA);
        Assert.Equal(1, hitB);
    }
}
