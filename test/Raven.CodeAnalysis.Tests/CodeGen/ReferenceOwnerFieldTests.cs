using System;
using System.IO;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public sealed class ReferenceOwnerFieldTests
{
    [Theory]
    [InlineData("let holder = Holder()\n holder.Item.Increment()\n holder.Item.Increment()\n return holder.Item.Value")]
    [InlineData("return Mutate(Holder())")]
    [InlineData("let box = Box()\n box.Owner.Item.Increment()\n box.Owner.Item.Increment()\n return box.Owner.Item.Value")]
    public void StructFieldMutationThroughReferenceOwnerUpdatesOriginalStorage(string body)
    {
        var source = """
struct Counter {
    public field Value: int = 40
    public func Increment() -> unit {
        Value = Value + 1
    }
}
class Holder {
    public field Item: Counter = Counter()
}
class Box {
    public field Owner: Holder = Holder()
}
class Program {
    static func Mutate(holder: Holder) -> int {
        holder.Item.Increment()
        holder.Item.Increment()
        return holder.Item.Value
    }
    public static func Run() -> int {
        BODY
    }
}
""".Replace("BODY", body);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("reference-owner-field",
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(references);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        Assert.Equal(42, loaded.Assembly.GetType("Program")!.GetMethod("Run")!.Invoke(null, null));
    }
    [Theory]
    [InlineData(true, OptimizationLevel.Debug)]
    [InlineData(false, OptimizationLevel.Debug)]
    [InlineData(true, OptimizationLevel.Release)]
    [InlineData(false, OptimizationLevel.Release)]
    public void ReferenceFieldAssignmentWithReturnPreservesReceiverOrder(bool exitEarly, OptimizationLevel optimization)
    {
        var source = """
public class Cell {
    public field Value: int = 7
}
public class Program {
    public static field Trace: int
    public static field Target: Cell = Cell()
    public static field Original: Cell = Target
    static func Receiver() -> Cell {
        Trace = Trace * 10 + 1
        return Target
    }
    public static func Run(exitEarly: bool) -> int {
        Receiver().Value = {
            Trace = Trace * 10 + 2
            Target = Cell()
            if exitEarly { return 42 }
            1
        }
        return Original.Value
    }
}
""";
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("reference-field-return",
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithOptimizationLevel(optimization))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(references);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var program = loaded.Assembly.GetType("Program")!;
        Assert.Equal(exitEarly ? 42 : 1, program.GetMethod("Run")!.Invoke(null, new object[] { exitEarly }));
        Assert.Equal(12, program.GetField("Trace")!.GetValue(null));
        var original = program.GetField("Original")!.GetValue(null)!;
        var target = program.GetField("Target")!.GetValue(null)!;
        Assert.NotSame(original, target);
        Assert.Equal(exitEarly ? 7 : 1, original.GetType().GetField("Value")!.GetValue(original));
        Assert.Equal(7, target.GetType().GetField("Value")!.GetValue(target));
    }

}
