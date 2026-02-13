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

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
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

    [Fact]
    public void ConditionalAccess_EventAdd_WiresHandler()
    {
        var code = """
interface Logger {
    Log(message: string)
}

class ConsoleLogger : Logger {
    public event Logged: System.Action<string>?

    public Log(message: string) -> unit {
        Logged?.Invoke(message)
    }
}

class Counter {
    public static Hits: int { get; set; }

    public static Increment() -> unit {
        Hits += 1
    }
}

class Program {
    public static Run() -> int {
        Counter.Hits = 0
        val logger = ConsoleLogger()

        logger?.Logged += (msg: string) => {
            Counter.Increment()
        }

        logger.Log("hello")
        return Counter.Hits
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Program", throwOnError: true)!;
        var runMethod = type.GetMethod("Run")!;
        var resultValue = runMethod.Invoke(null, Array.Empty<object>())!;

        Assert.Equal(1, resultValue);
    }
}
