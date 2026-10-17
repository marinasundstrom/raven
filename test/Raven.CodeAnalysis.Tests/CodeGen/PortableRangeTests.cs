using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class PortableRangeTests
{
    [Theory]
    [InlineData("2147483646..2147483647", 1)]
    [InlineData("2147483647..2147483647", 0)]
    [InlineData("2147483647..2147483646 by 1", 0)]
    [InlineData("-2147483648..-2147483646 by 1", 2)]
    [InlineData("9223372036854775806L..9223372036854775807L", 1)]
    [InlineData("9223372036854775807L..9223372036854775807L", 0)]
    [InlineData("9223372036854775807L..9223372036854775806L by 1L", 0)]
    [InlineData("-9223372036854775808L..-9223372036854775806L by 1L", 2)]
    public void ExclusiveUnitRangesPreserveSignedBoundaries(string range, int expected)
    {
        var source = $$"""
            public static class Ranges {
                public static func Run() -> int {
                    var count = 0
                    for value in {{range}} {
                        count += 1
                        if count > 2 { return -1 }
                        continue
                    }
                    return count
                }
            }
            """;
        var compilation = Compilation.Create("PortableUnitRanges" + Guid.NewGuid().ToString("N"),
            [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var tree = compilation.SyntaxTrees[0];
        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            allowsRangeEnumeration: true);
        Assert.True(LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(method)!, model, method.Body!,
            _ => false, out _, out var failure, capabilities), failure?.Detail);
        using var stream = new MemoryStream();
        var emitted = compilation.Emit(stream);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        Assert.Equal(expected, Assembly.Load(stream.ToArray()).GetType("Ranges")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [InlineData("int", "1..3", 0, false)]
    [InlineData("int", "1..3", 1, true)]
    [InlineData("int", "1..3", 3, false)]
    [InlineData("int", "1..=3", 3, true)]
    [InlineData("int", "1..=3", 4, false)]
    [InlineData("int", "..3", 2, true)]
    [InlineData("int", "..3", 3, false)]
    [InlineData("int", "3..", 3, true)]
    [InlineData("int", "3..1", 2, false)]
    [InlineData("long", "1L..=3L", 4294967297L, false)]
    [InlineData("long", "1L..=3L", 3, true)]
    [InlineData("long", "4294967296..=4294967298", 4294967298L, true)]
    [InlineData("long", "4294967296..4294967298", 4294967298L, false)]
    public void SignedRangePatternsPreserveEndpointAndWidth(string type, string pattern, long value, bool expected)
    {
        var source = $$"""
            public static class Ranges {
                public static func Run(value: {{type}}) -> bool {
                    if value is {{pattern}} { return true }
                    return false
                }
                public static func Matches(value: {{type}}) -> bool => value is {{pattern}}
            }
            """;
        var compilation = Compilation.Create("PortableRangePatterns" + Guid.NewGuid().ToString("N"),
            [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var tree = compilation.SyntaxTrees[0];
        var model = compilation.GetSemanticModel(tree);
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsCasePatterns: true);
        foreach (var method in tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>())
        {
            Assert.True(SourceCallablePlan.TryCreate((IMethodSymbol)model.GetDeclaredSymbol(method)!, out var plan, capabilities));
            Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, capabilities), failure?.Detail);
        }
        using var stream = new MemoryStream();
        var emitted = compilation.Emit(stream);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        object argument = type == "int" ? (object)(int)value : value;
        var emittedType = Assembly.Load(stream.ToArray()).GetType("Ranges")!;
        Assert.Equal(expected, emittedType.GetMethod("Run")!.Invoke(null, [argument]));
        Assert.Equal(expected, emittedType.GetMethod("Matches")!.Invoke(null, [argument]));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void SignedRangesRequireCapabilityAndPreserveDotNetResults(bool enabled)
    {
        const string source = """
            public static class Ranges {
                public static func Run(zero: int) -> int {
                    var total = 0
                    for value in 1..=5 by 2 { total += value }
                    for value in 5..=1 by -2 { total += value }
                    for value in 0..3 { total += value }
                    for value in 3..0 by -1 { total += value }
                    for value in 1..=3 by zero { return 1 }
                    for value in 1..=3 by -1 { return 2 }
                    outer: for value in 1..=3 {
                        for inner in 1..=3 {
                            if inner == 2 { continue outer }
                            total += 1
                        }
                    }
                    for value in 1..=10 {
                        if value == 1 { continue }
                        if value == 3 { break }
                        total += value
                    }
                    let start: long = 4294967296
                    let end: long = 4294967300
                    let step: long = 2
                    for value in start..end by step { total += 5 }
                    return total
                }
            }
            """;
        var compilation = Compilation.Create("PortableRanges" + Guid.NewGuid().ToString("N"),
            [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var tree = compilation.SyntaxTrees[0];
        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            allowsRangeEnumeration: enabled);
        var success = LinearMethodBody.TryLower((IMethodSymbol)model.GetDeclaredSymbol(method)!, model, method.Body!,
            _ => false, out _, out var failure, capabilities);
        Assert.True(success == enabled, failure?.Detail);
        using var stream = new MemoryStream();
        var emitted = compilation.Emit(stream);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        Assert.Equal(42, Assembly.Load(stream.ToArray()).GetType("Ranges")!.GetMethod("Run")!.Invoke(null, [0]));
    }
}
