using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class EnumInstanceReceiverExecutionTests
{
    [Theory]
    [InlineData("return value.ToString()", "Friday")]
    [InlineData("return value.ToString().ToLowerInvariant()", "friday")]
    [InlineData("return value.ToString(\"D\")", "5")]
    [InlineData("return value.GetHashCode().ToString()", "5")]
    [InlineData("let copy = value; return copy.ToString()", "Friday")]
    [InlineData("return System.DayOfWeek.Friday.ToString()", "Friday")]
    [InlineData("return GetDay().ToString()", "Friday")]
    [InlineData("return Day.ToString()", "Friday")]
    [InlineData("return value.HasFlag(System.DayOfWeek.Monday).ToString()", "True")]
    [InlineData("return value.GetType().Name", "DayOfWeek")]
    [InlineData("return Stored.ToString()", "Friday")]
    public void DotNet_ImportedEnumInstanceCallsPreserveReceiverValue(string body, string expected)
    {
        var tree = SyntaxTree.ParseText($$"""
            class Probe {
                static field Stored: System.DayOfWeek = System.DayOfWeek.Friday
                static val Day: System.DayOfWeek => System.DayOfWeek.Friday
                static func GetDay() -> System.DayOfWeek => System.DayOfWeek.Friday
                static func Run(value: System.DayOfWeek) -> string { {{body}} }
            }
            """);
        var references = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"))
            .Select(path => MetadataReference.CreateFromFile(path)).ToArray();
        var compilation = Compilation.Create("EnumReceiver", [tree], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithFrameworkProjectionMode(FrameworkProjectionMode.None));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var run = loaded.Assembly.GetType("Probe")!.GetMethod("Run")!;
        Assert.Equal(expected, run.Invoke(null, [DayOfWeek.Friday]));
    }
}
