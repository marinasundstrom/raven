using Raven.CodeAnalysis.Syntax;
namespace Raven.CodeAnalysis.Tests;

public sealed class OpenClassInterfacePatternTests
{
    [Theory]
    [InlineData("open class Base {}", true)]
    [InlineData("abstract class Base {}", true)]
    [InlineData("interface Base {}", true)]
    [InlineData("class Base {}", false)]
    [InlineData("class Base : Marker {}", true)]
    [Trait("Target", "DotNet")]
    public void DotNet_PatternsRespectPossibleRuntimeInterfaceImplementations(string declaration, bool possible)
        => Verify(TargetPlatform.DotNet, declaration, possible);

    [Theory]
    [InlineData("open class Base {}", true)]
    [InlineData("abstract class Base {}", true)]
    [InlineData("interface Base {}", true)]
    [InlineData("class Base {}", false)]
    [InlineData("class Base : Marker {}", true)]
    [Trait("Target", "NeoCLR")]
    public void NeoCLR_PatternsRespectPossibleRuntimeInterfaceImplementations(string declaration, bool possible)
        => Verify(TargetPlatform.NeoCLR, declaration, possible);

    private static void Verify(TargetPlatform target, string declaration, bool possible)
    {
        var source = "interface Marker {}\n" + declaration + "\nclass Probe {\nstatic func Test(value: Base) -> bool => match value { Marker => true _ => false }\n}";
        var compilation = Create(source, target);
        // Host-reference semantic queries do not claim neoCLR emission or execution.
        var diagnostics = compilation.GetSemanticModel(compilation.SyntaxTrees.Single()).GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (possible) Assert.Empty(diagnostics);
        else Assert.Contains(diagnostics, d => d.Id == "RAV2102");
        var baseType = compilation.GetTypeByMetadataName("Base")!;
        var marker = compilation.GetTypeByMetadataName("Marker")!;
        foreach (var (from, to) in new[] { (baseType, marker), (marker, baseType) })
        {
            var conversion = compilation.ClassifyConversion(from, to, includeUserDefined: false);
            Assert.Equal(possible, conversion.Exists);
            if (possible) Assert.True(conversion.IsReference);
        }
    }

    [Fact]
    [Trait("Target", "DotNet")]
    public void DotNet_MatchTestsTheDerivedRuntimeObject()
    {
        var compilation = Create("""
            interface Marker {}
            open class Base {}
            class Hit : Base, Marker {}
            class Miss : Base {}
            class Probe {
                static func Cast(value: Base) -> Marker => (Marker)value
                static func Reverse(value: Marker) -> Base => (Base)value
                static func Test(value: Base) -> bool => match value {
                    Marker => true
                    _ => false
                }
            }
            """, TargetPlatform.DotNet);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var test = loaded.Assembly.GetType("Probe")!.GetMethod("Test")!;
        var hit = Activator.CreateInstance(loaded.Assembly.GetType("Hit")!);
        Assert.Equal(true, test.Invoke(null, [hit]));
        var cast = loaded.Assembly.GetType("Probe")!.GetMethod("Cast")!;
        var reverse = loaded.Assembly.GetType("Probe")!.GetMethod("Reverse")!;
        Assert.Same(hit, cast.Invoke(null, [hit]));
        Assert.Same(hit, reverse.Invoke(null, [hit]));
        var error = Assert.Throws<System.Reflection.TargetInvocationException>(() => cast.Invoke(null, [Activator.CreateInstance(loaded.Assembly.GetType("Miss")!)]));
        Assert.IsType<InvalidCastException>(error.InnerException);
        Assert.Equal(false, test.Invoke(null, [Activator.CreateInstance(loaded.Assembly.GetType("Miss")!)]));
    }

    private static Compilation Create(string source, TargetPlatform target)
        => Compilation.Create("OpenInterfacePatterns", [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
}
