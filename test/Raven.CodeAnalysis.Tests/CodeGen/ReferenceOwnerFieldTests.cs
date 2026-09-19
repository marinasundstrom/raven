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
}
