using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class ConditionalExtensionReceiverExecutionTests
{
    [Theory]
    [InlineData("Number", null, false, 0)]
    [InlineData("Number", 0, false, 1)]
    [InlineData("Number", 3, true, 1)]
    [InlineData("GetNumber()", null, false, 0)]
    [InlineData("GetNumber()", 0, false, 1)]
    [InlineData("GetNumber()", 3, true, 1)]
    public void ConditionalExtensionEvaluatesIntermediateReceiverOnce(string member, int? input, bool expected, int reads)
    {
        var tree = SyntaxTree.ParseText($$"""
            import Raven.CodeAnalysis.Tests.*
            class Probe {
                static func Run(value: ConditionalExtensionFixture?) -> bool => value?.{{member}}.IsPositive() == true
            }
            """);
        var references = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"))
            .Select(MetadataReference.CreateFromFile)
            .Append(MetadataReference.CreateFromFile(typeof(ConditionalExtensionFixture).Assembly.Location)).ToArray();
        var compilation = Compilation.Create("ConditionalExtensionReceiver", [tree], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, references);
        var probe = loaded.Assembly.GetType("Probe")!;
        var fixture = input is int number ? new ConditionalExtensionFixture(number) : null;
        Assert.Equal(expected, probe.GetMethod("Run")!.Invoke(null, [fixture]));
        Assert.Equal(reads, fixture?.Reads ?? 0);
    }
}

public sealed class ConditionalExtensionFixture(int number)
{
    public int Reads { get; private set; }
    public int Number { get { Reads++; return number; } }
    public int GetNumber() { Reads++; return number; }
}

public static class ConditionalExtensionFixtureExtensions
{
    public static bool IsPositive(this int value) => value > 0;
}
